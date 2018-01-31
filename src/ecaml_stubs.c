#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "emacs-module.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>

#include "ecaml_stubs.h"

int plugin_is_GPL_compatible;

/* [active_env] is the currently active Emacs environment.  It is only correct to look at
   it and set it from the Emacs thread.  It is set for the duration of a call from Emacs
   to OCaml, in the [Fdispatch] function.  It is NULL when we're not in a call from Emacs
   to OCaml. */
static emacs_env *active_env = NULL;

#define CAML_CALLBACK_1 caml_callback
#define CAML_CALLBACK_2 caml_callback2
#define CAML_CALLBACK_3 caml_callback3
#define CAML_CALLBACK_1_exn caml_callback_exn
#define CAML_CALLBACK_2_exn caml_callback2_exn
#define CAML_CALLBACK_3_exn caml_callback3_exn

#define CAML_NAMED_CALLBACK(val, name, arity_exn, ...)                        \
  do {                                                                        \
    static value *name = NULL;                                                \
    if (name == NULL) {                                                       \
      name = caml_named_value(#name);                                         \
      if (name == NULL) {                                                     \
        fprintf(stderr, "caml_named_value(\"%s\") returned NULL!\n", #name);  \
        exit(1);                                                              \
      }                                                                       \
    }                                                                         \
    val = CAML_CALLBACK_##arity_exn(*name, __VA_ARGS__);                      \
  } while (0)

// We don't use the CAML macros here we don't care about the returned
// value so it's not an issue if it gets moved/collected.
#define CAML_NAMED_CALLBACK_I(name, arity_exn, ...)            \
  do {                                                         \
    __attribute__((unused)) value bogus;                       \
    CAML_NAMED_CALLBACK(bogus, name, arity_exn, __VA_ARGS__);  \
  } while (0)

emacs_env* ecaml_active_env_or_die()
{
  if (active_env == NULL) {
    CAML_NAMED_CALLBACK_I(no_active_env, 1, Val_unit);
    exit(1);
  }
  return active_env;
}

/* We suspect we can only use env->free_global_ref from inside the emacs thread, so
   when the ocaml gc finds out a dead emacs value, we can't just free it right here since
   we're possibly not in the emacs thread. Instead we simply push the values to free in a
   stack, and then anytime emacs calls us, we free everything that was scheduled. We also
   free before returning to emacs, since the ocaml gc has possibly run in the meantime and
   found more emacs values to free.
   The use of these variables must be protected by the ocaml runtime lock. */
static emacs_value* to_free = NULL;
static long to_free_size = 0;
static long to_free_index = 0;
static long num_emacs_free_scheduled = 0;
static long num_emacs_free_performed = 0;

CAMLprim value ecaml_num_emacs_free_scheduled(value unit) {
  (void) unit;
  return Val_long(num_emacs_free_scheduled);
}

CAMLprim value ecaml_num_emacs_free_performed(value unit) {
  (void) unit;
  return Val_long(num_emacs_free_performed);
}

static void push_to_free(emacs_value v) {
  if (to_free_size == to_free_index) {
    to_free_size = 2 * to_free_size + 1;
    to_free = realloc(to_free, sizeof(emacs_value) * to_free_size);
  }
  to_free[to_free_index] = v;
  to_free_index++;
  num_emacs_free_scheduled++;
}

static void free_emacs_values_assuming_in_emacs_thread(emacs_env *env) {
  if (env != NULL) {
    while (to_free_index != 0) {
      to_free_index--;
      env->free_global_ref(env, to_free[to_free_index]);
      num_emacs_free_performed++;
    }
  }
}

// extract wrapped emacs_value. also lvalue.
#define Emacs_val(val) *(emacs_value *)Data_custom_val(val)

static void finalize_emacs_value(value val) {
  // val is not an int since it was finalized
  emacs_value the_val = Emacs_val(val);
  push_to_free(the_val);
}

static struct custom_operations emacs_value_ops = {
  .identifier  = "ecaml/emacs_value",
  .finalize    = finalize_emacs_value,
  .compare     = custom_compare_default,
  .compare_ext = custom_compare_ext_default,
  .hash        = custom_hash_default,
  .serialize   = custom_serialize_default,
  .deserialize = custom_deserialize_default,
};

// caml_nil cannot conflict with any ordinary [Value.t] because it is an immediate value.
static value caml_nil = Val_unit;
static const emacs_value emacs_nil = (emacs_value)NULL;

emacs_value ecaml_cache_symbol_and_keep_alive(emacs_env* env, emacs_value *cache, char* symbol) {
  if (*cache == NULL) {
    *cache = env->make_global_ref(env, env->intern(env, symbol));
  }
  return *cache;
}

/* [ecaml_emacs_t] is used in generated stubs when an input argument is an OCaml [bool] in
   order to avoid unnecessarily allocation of [emacs_value]'s pointing to [t]. */
emacs_value ecaml_emacs_t(emacs_env* env) {
  static emacs_value cache = NULL;
  return ecaml_cache_symbol_and_keep_alive(env, &cache, "t");
}

static value ecaml_nil()
{
  /* We allocate [caml_nil] the first time it is used. */
  if (Is_long(caml_nil)) {
    caml_register_generational_global_root(&caml_nil);
    caml_modify_generational_global_root(&caml_nil, caml_alloc_custom(&emacs_value_ops, sizeof(emacs_value), 0, 1));
    Emacs_val(caml_nil) = emacs_nil;
  }
  return caml_nil;
}

static bool is_emacs_integer(emacs_env* env, emacs_value val) {
  static emacs_value cache = NULL;
  emacs_value integer_type = ecaml_cache_symbol_and_keep_alive(env, &cache, "integer");
  return (env->type_of(env, val) == integer_type);
}

static value ocaml_of_emacs(emacs_env* env, emacs_value val) {
  CAMLparam0();
  CAMLlocal1(v);
  if (val == emacs_nil) {
    v = ecaml_nil();
  } else if (is_emacs_integer(env, val)) {
    /* This is ok because emacs integers range is subset of OCaml integer range and we
       have tests for it. */
    v = Val_long(env->extract_integer(env, val));
  } else {
    v = caml_alloc_custom(&emacs_value_ops, sizeof(emacs_value), 0, 1);
    Emacs_val(v) = env->make_global_ref(env, val);
  }
  CAMLreturn(v);
}

emacs_value emacs_of_ocaml(emacs_env* env, value val) {
  emacs_value ret;
  if (Is_block(val)) {
    ret = Emacs_val(val);
  } else {
    /* If [val] is an int, it either came from emacs or it was created by
       [Value.of_int_exn] which checks bounds. In either case, it is within the range of
       emacs integers so [env->make_integer] shouldn't fail. */
    ret = env->make_integer(env, Long_val(val));
  }
  return ret;
}

CAMLprim value
ecaml_have_active_env(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_bool(active_env != NULL));
}

/* [env->non_local_exit_get] checks a [pending_error] value in the environment that ends
   up being set by many of the [env] functions.  For robustness, in [value.ml], we follow
   all calls to [env] functions with a call to [ecaml_non_local_exit_get_and_clear]. */
CAMLprim value
ecaml_non_local_exit_get_and_clear(value unit)
{
  CAMLparam1(unit);
  CAMLlocal3(ret, symbol, data);

  emacs_env* env = ecaml_active_env_or_die();

  emacs_value non_local_exit_symbol;
  emacs_value non_local_exit_data;
  enum emacs_funcall_exit non_local_exit =
    env->non_local_exit_get(env, &non_local_exit_symbol, &non_local_exit_data);

  env->non_local_exit_clear(env);

  switch (non_local_exit) {
  case emacs_funcall_exit_return:        // Return
    ret = Val_long(0);
    break;
  case emacs_funcall_exit_signal:        // Signal (symbol, data)
    symbol = ocaml_of_emacs(env, non_local_exit_symbol);
    data = ocaml_of_emacs(env, non_local_exit_data);
    ret = caml_alloc(2, 0);
    Store_field(ret, 0, symbol);
    Store_field(ret, 1, data);
    break;
  case emacs_funcall_exit_throw:         // Throw (tag, value)
    symbol = ocaml_of_emacs(env, non_local_exit_symbol);
    data = ocaml_of_emacs(env, non_local_exit_data);
    ret = caml_alloc(2, 1);
    Store_field(ret, 0, symbol);
    Store_field(ret, 1, data);
    break;
  }

  CAMLreturn(ret);
}

CAMLprim value
ecaml_non_local_exit_signal(value symbol, value data)
{
  CAMLparam2(symbol, data);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value non_local_exit_symbol = emacs_of_ocaml(env, symbol);
  emacs_value non_local_exit_data = emacs_of_ocaml(env, data);
  env->non_local_exit_signal(env, non_local_exit_symbol, non_local_exit_data);
  CAMLreturn(Val_unit);
}

/* This should be used in every place where Emacs calls OCaml and we use [caml_callback]
   (i.e. the place where we start by acquiring the OCaml lock) along with using the _exn
   variant of [caml_callback] (which is the one that doesn't raise), so that on exception,
   OCaml doesn't unwind the stack past the Emacs code, or just exit without giving Emacs a
   chance to do whatever cleanup it needs to do.  Regardless of this we could still raise
   [Out_of_memory] with every allocation, but there is no documented way to either
   allocate OCaml blocks without raising [Out_of_memory] or catch OCaml exceptions from
   C. */
static void if_exception_signal_emacs_and_use_ecaml_nil(emacs_env* maybe_env, char* symbol_string, value *ret) {
  if(Is_exception_result(*ret)) {
    *ret = Val_unit; /* I don't know if we can change the set of roots (in emacs_nil())
                        while *ret contains an exception result, so let's make it contain
                        some random valid value instead to be on the safe side. */
    *ret = ecaml_nil();
    if (maybe_env != NULL) {
      emacs_env *env = maybe_env;
      /* Ignoring the exception is not great, but it only happens in free_caml_cb, which
         will not raise as currently implemented. */
      env->non_local_exit_signal(env,
                                 env->intern(env, "uncaught-ocaml-exception"),
                                 env->intern(env, symbol_string));
    }
  }
}

static emacs_value
Fdispatch_assuming_lock_is_held(emacs_env* env, ptrdiff_t nargs, emacs_value args[],
                                __attribute__((unused)) void *data)
{
  CAMLparam0();
  CAMLlocal4(callback_id, arg_array, ret, tmp);

  /* shouldn't never really happen since Fdispatch is not bound to any symbol */
  if (nargs <= 0) {
    /* error wrong-number-of-arguments requires a list of two elements, function symbol
       and the number of arguments */
    emacs_value list_fun = env->intern(env, "list");
    emacs_value list_elems[] = { env->intern(env, "Fdispatch"),
                                 env->make_integer(env, nargs) };
    emacs_value signal_data = env->funcall(env, list_fun, 2, list_elems);

    env->non_local_exit_signal
      (env, env->intern(env, "wrong-number-of-arguments"), signal_data);

    CAMLreturnT(emacs_value, emacs_nil);
  }

  callback_id = *((value *)env->get_user_ptr(env, args[0]));
  nargs--; args++;

  if (nargs == 0) {
    // A zero-length array.  The OCaml manual says not to use [caml_alloc] to allocate a
    // zero-length block.
    arg_array = Atom(0);
  } else {
    arg_array = caml_alloc(nargs, 0);
    for (int i = 0; i < nargs; i++) {
      tmp = ocaml_of_emacs(env, args[i]);
      Store_field(arg_array, i, tmp);
    }
  }

  CAML_NAMED_CALLBACK(ret, dispatch_function, 2_exn, callback_id, arg_array);
  if_exception_signal_emacs_and_use_ecaml_nil(env, "Fdispatch", &ret);

  emacs_value ret_val = emacs_of_ocaml(env, ret);

  CAMLreturnT(emacs_value, ret_val);
}

//forward
static void free_embedded_caml_values(emacs_env*);

typedef struct {
  bool need_to_lock_caml;
  emacs_env *old_env;
} acquire_lock;

static acquire_lock acquire_ocaml_lock_from_emacs(emacs_env *env) {
  /* Emacs can call us both as a response to us calling it, in which case we already have
     the OCaml lock, or in response to user input, in which case we need to take the lock.
     The OCaml lock is not recursive, we just deadlock is we try to acquire it again, and
     the OCaml runtime does not provide a way of checking whether we hold the lock, so we
     use [active_env] to check whether we have the lock. */
  bool need_to_lock_caml = active_env == NULL;

  if (need_to_lock_caml) {
    caml_acquire_runtime_system();
  }
  free_emacs_values_assuming_in_emacs_thread(env);
  free_embedded_caml_values(env);

  acquire_lock acquire_lock = {
    .need_to_lock_caml = need_to_lock_caml,
    .old_env = active_env
  };

  active_env = env;

  return acquire_lock;
}

static void release_ocaml_lock_from_emacs(acquire_lock acquire_lock, emacs_env *env) {
  active_env = acquire_lock.old_env;
  free_emacs_values_assuming_in_emacs_thread(env);
  if (acquire_lock.need_to_lock_caml) {
    caml_release_runtime_system();
  }
}

// [Fdispatch] and [free_caml_cb] are the only functions that we use for calling from
// Emacs to OCaml.
static emacs_value
Fdispatch(emacs_env *env,
          ptrdiff_t nargs,
          emacs_value args[],
          void *data)
{
  acquire_lock lock_state = acquire_ocaml_lock_from_emacs(env);
  emacs_value ret;

  ret = Fdispatch_assuming_lock_is_held(env, nargs, args, data);

  release_ocaml_lock_from_emacs(lock_state, env);

  return ret;
}

int emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);
  assert(active_env == NULL);
  active_env = env;

  // We assume that any emacs_value we get that is equal to NULL represents the value
  // [nil]. Emacs's [emacs-module.c] tells us not to assume this, so we just check it at
  // startup.
  assert(env->intern(env, "nil") == NULL);

  emacs_value Qargv = env->intern(env, "argv");
  emacs_value list = env->funcall(env, env->intern(env, "symbol-value"), 1, &Qargv);
  emacs_value vect = env->funcall(env, env->intern(env, "vconcat"), 1, &list);
  int argc = env->vec_size(env, vect);
  char *argv[argc + 1];

  for (int i = 0; i < argc; i++) {
    emacs_value arg = env->vec_get(env, vect, i);
    ptrdiff_t size = 0;
    env->copy_string_contents(env, arg, NULL, &size);
    argv[i] = (char *)malloc(size * sizeof(char));
    env->copy_string_contents(env, arg, argv[i], &size);
  }

  argv[argc] = NULL;

  // [caml_startup] acquires the OCaml lock.
  caml_startup(argv);

  CAML_NAMED_CALLBACK_I(end_of_module_initialization, 1, Val_unit);

  active_env = NULL;
  caml_release_runtime_system();
  return 0;
}

CAMLprim value
ecaml_make_dispatch_function(value doc)
{
  CAMLparam1(doc);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  /* [make_function] doesn't deal with NULs in OCaml strings. */
  emacs_value emacs_func =
    env->make_function(env, 0, emacs_variadic_function, Fdispatch,
                       String_val(doc), NULL);
  ret = ocaml_of_emacs(env, emacs_func);
  CAMLreturn(ret);
}

value ecaml_funcall(emacs_env* env, value fun, emacs_value *args, size_t nargs, value should_return_result)
{
  CAMLparam2(fun, should_return_result);
  CAMLlocal1(ret);
  emacs_value the_fun = emacs_of_ocaml(env, fun);
  emacs_value ret_val = env->funcall(env, the_fun, nargs, args);
  if (Bool_val(should_return_result)) {
    ret = ocaml_of_emacs(env, ret_val);
  } else {
    ret = ecaml_nil();
  }
  CAMLreturn(ret);
}

CAMLprim value
ecaml_funcall_array(value fun, value args, value should_return_result)
{
  CAMLparam3(fun, args, should_return_result);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  size_t nargs = Wosize_val(args);

  // If [nargs > cutoff], we use malloc/free to allocate [the_args], whereas if [nargs <=
  // cutoff], we use a stack-allocated array instead.
  const size_t cutoff = 64;
  emacs_value arg_array[cutoff];
  emacs_value *the_args;

  if (nargs > cutoff) {
    the_args = (emacs_value *)malloc(nargs * sizeof(emacs_value));
  } else {
    the_args = arg_array;
  }

  for (size_t i = 0; i < nargs; i++) {
    the_args[i] = emacs_of_ocaml(env, Field(args, i));
  }
  ret = ecaml_funcall(env, fun, the_args, nargs, should_return_result);
  if (nargs > cutoff) {
    free(the_args);
  }
  CAMLreturn(ret);
}

CAMLprim value
ecaml_funcall0(value fun, value should_return_result)
{
  emacs_env* env = ecaml_active_env_or_die();
  return ecaml_funcall(env, fun, NULL, 0, should_return_result);
}

CAMLprim value
ecaml_funcall1(value fun, value arg0, value should_return_result)
{
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_arg;
  the_arg = emacs_of_ocaml(env, arg0);
  return ecaml_funcall(env, fun, &the_arg, 1, should_return_result);
}

CAMLprim value
ecaml_funcall2(value fun, value arg0, value arg1, value should_return_result)
{
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_args[2];
  the_args[0] = emacs_of_ocaml(env, arg0);
  the_args[1] = emacs_of_ocaml(env, arg1);
  return ecaml_funcall(env, fun, the_args, 2, should_return_result);
}

CAMLprim value
ecaml_funcall3(value fun, value arg0, value arg1, value arg2, value should_return_result)
{
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_args[3];
  the_args[0] = emacs_of_ocaml(env, arg0);
  the_args[1] = emacs_of_ocaml(env, arg1);
  the_args[2] = emacs_of_ocaml(env, arg2);
  return ecaml_funcall(env, fun, the_args, 3, should_return_result);
}

CAMLprim value
ecaml_funcall4(value fun, value arg0, value arg1, value arg2, value arg3, value should_return_result)
{
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_args[4];
  the_args[0] = emacs_of_ocaml(env, arg0);
  the_args[1] = emacs_of_ocaml(env, arg1);
  the_args[2] = emacs_of_ocaml(env, arg2);
  the_args[3] = emacs_of_ocaml(env, arg3);
  return ecaml_funcall(env, fun, the_args, 4, should_return_result);
}

CAMLprim value
ecaml_funcall_int_int_value_unit(value fun, value arg0, value arg1, value arg2)
{
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_args[3];
  the_args[0] = env->make_integer(env, Long_val(arg0));
  the_args[1] = env->make_integer(env, Long_val(arg1));
  the_args[2] = emacs_of_ocaml(env, arg2);
  return ecaml_funcall(env, fun, the_args, 3, false);
}

CAMLprim value
ecaml_funcall_int_int_value_value_unit(value fun, value arg0, value arg1, value arg2, value arg3)
{
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_args[4];
  the_args[0] = env->make_integer(env, Long_val(arg0));
  the_args[1] = env->make_integer(env, Long_val(arg1));
  the_args[2] = emacs_of_ocaml(env, arg2);
  the_args[3] = emacs_of_ocaml(env, arg3);
  return ecaml_funcall(env, fun, the_args, 4, false);
}

CAMLprim value
ecaml_funcall5(value fun, value arg0, value arg1, value arg2, value arg3, value arg4, value should_return_result)
{
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_args[5];
  the_args[0] = emacs_of_ocaml(env, arg0);
  the_args[1] = emacs_of_ocaml(env, arg1);
  the_args[2] = emacs_of_ocaml(env, arg2);
  the_args[3] = emacs_of_ocaml(env, arg3);
  the_args[4] = emacs_of_ocaml(env, arg4);
  return ecaml_funcall(env, fun, the_args, 5, should_return_result);
}

CAMLprim value
ecaml_funcall4_byte(value *argv, int argn)
{
  assert(argn == 6);
  return ecaml_funcall4(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value
ecaml_funcall5_byte(value *argv, int argn)
{
  assert(argn == 7);
  return ecaml_funcall5(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value
ecaml_intern(value symbol_name)
{
  CAMLparam1(symbol_name);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  /* ok because we don't trigger CAML gc below */
  char *the_symbol_name = String_val(symbol_name);
  emacs_value ret_val = env->intern(env, the_symbol_name);
  ret = ocaml_of_emacs(env, ret_val);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_type_of(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_val = emacs_of_ocaml(env, val);
  emacs_value ret_val = env->type_of(env, the_val);
  ret = ocaml_of_emacs(env, ret_val);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_is_not_nil(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_val = emacs_of_ocaml(env, val);
  ret = Val_bool(env->is_not_nil(env, the_val));
  CAMLreturn(ret);
}

CAMLprim value
ecaml_eq(value a, value b)
{
  CAMLparam2(a, b);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_a = emacs_of_ocaml(env, a);
  emacs_value the_b = emacs_of_ocaml(env, b);
  ret = Val_bool(env->eq(env, the_a, the_b));
  CAMLreturn(ret);
}

CAMLprim value
ecaml_to_float(value val)
{
  CAMLparam1(val);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_val = emacs_of_ocaml(env, val);
  double ret_val = env->extract_float(env, the_val);
  CAMLreturn(caml_copy_double(ret_val));
}

CAMLprim value
ecaml_of_float(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  double the_val = Double_val(val);
  emacs_value ret_val = env->make_float(env, the_val);
  ret = ocaml_of_emacs(env, ret_val);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_to_string(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_val = emacs_of_ocaml(env, val);
  ptrdiff_t size = 0;
  env->copy_string_contents(env, the_val, NULL, &size);
  /* [copy_string_contents] writes [size] including space for a NUL byte at the end.
     Since [caml_alloc_string] allocates enough space for a NUL byte at the end anyway, we
     pass [size - 1] to [caml_alloc_string] and it is safe to allow [copy_string_contents]
     to blit into this buffer (since the last byte is NUL). */
  ret = caml_alloc_string(size - 1);
  env->copy_string_contents(env, the_val, String_val(ret), &size);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_of_string(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  char *buf = String_val(val);
  int len = caml_string_length(val);
  emacs_value ret_val = env->make_string(env, buf, len);
  ret = ocaml_of_emacs(env, ret_val);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_vec_get(value vec, value index)
{
  CAMLparam2(vec, index);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_vec = emacs_of_ocaml(env, vec);
  long the_index = Long_val(index);
  emacs_value ret_val = env->vec_get(env, the_vec, the_index);
  ret = ocaml_of_emacs(env, ret_val);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_vec_set(value vec, value index, value val)
{
  CAMLparam3(vec, index, val);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_vec = emacs_of_ocaml(env, vec);
  long the_index = Long_val(index);
  emacs_value the_val = emacs_of_ocaml(env, val);
  env->vec_set(env, the_vec, the_index, the_val);
  CAMLreturn(Val_unit);
}

CAMLprim value
ecaml_vec_size(value vec)
{
  CAMLparam1(vec);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value the_vec = emacs_of_ocaml(env, vec);
  ptrdiff_t ret_val = env->vec_size(env, the_vec);
  CAMLreturn(Val_long(ret_val));
}

static value* caml_embedded_ids_to_free = NULL;
static long caml_embedded_ids_to_free_size = 0;
static long caml_embedded_ids_to_free_index = 0;

/* This function runs in the middle of emacs GC so we avoid calling into OCaml and simply
   just store the [caml_embedded_id] to be processed later by
   [free_embedded_caml_values].

   Note that all [value]s below are [Caml_embedded_id.t] which is an immediate so we don't
   have to worry about GC.
*/
static void finalize_caml_embedded(void* data)
{
  if (caml_embedded_ids_to_free_size == caml_embedded_ids_to_free_index) {
    caml_embedded_ids_to_free_size = 2 * caml_embedded_ids_to_free_size + 1;
    caml_embedded_ids_to_free =
      realloc(caml_embedded_ids_to_free, sizeof(value) * caml_embedded_ids_to_free_size);
  }
  value caml_embedded_id = *((value *)data);
  caml_embedded_ids_to_free[caml_embedded_ids_to_free_index++] = caml_embedded_id;
  free(data);
}

void free_embedded_caml_values(emacs_env* env)
{
  CAMLparam0();
  CAMLlocal2(ids,ret);
  if (caml_embedded_ids_to_free_index > 0) {
    ids = caml_alloc (caml_embedded_ids_to_free_index, 0);
    memcpy(Op_val(ids), caml_embedded_ids_to_free,
           caml_embedded_ids_to_free_index * sizeof(value));
    caml_embedded_ids_to_free_index = 0;
    CAML_NAMED_CALLBACK(ret, free_embedded_caml_values, 1_exn, ids);
    if_exception_signal_emacs_and_use_ecaml_nil(env, "free_embedded_caml_values", &ret);
  }
  CAMLreturn0;
}

CAMLprim value ecaml_inject(value caml_embedded_id)
{
  CAMLparam1(caml_embedded_id);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  value* data = malloc(sizeof(value));
  *data = caml_embedded_id;
  emacs_value user_ptr = env->make_user_ptr(env, finalize_caml_embedded, (void *)data);
  ret = ocaml_of_emacs(env, user_ptr);
  CAMLreturn(ret);
}

CAMLprim value ecaml_project(value val)
{
  CAMLparam1(val);
  emacs_env* env = ecaml_active_env_or_die();
  emacs_value user_ptr = emacs_of_ocaml(env, val);
  void* data = env->get_user_ptr(env, user_ptr);
  CAMLreturn(*((value *)data));
}
