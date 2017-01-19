#define _GNU_SOURCE

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

#include "emacs-module.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>

int plugin_is_GPL_compatible;

/* [active_env] is the currently active Emacs environment, defined by the currently active
   call from Emacs to OCaml, set via the [dispatch] function.  It is undefined when we're
   not in a call from Emacs to OCaml. */
static emacs_env *active_env = NULL;

#define INTERN(symbol_name) active_env->intern(active_env, symbol_name)
#define FUNCALL(fun, nargs, args) active_env->funcall(active_env, fun, nargs, args)

#define THREAD_ID syscall(SYS_gettid)

#define CAML_CALLBACK_1 caml_callback
#define CAML_CALLBACK_2 caml_callback2
#define CAML_CALLBACK_3 caml_callback3

#define CAML_NAMED_CALLBACK(val, name, argc, ...)                       \
  do {                                                                  \
    static value *name = NULL;                                          \
    if (name == NULL) {                                                 \
      name = caml_named_value(#name);                                   \
      if (name == NULL) {                                               \
        fprintf(stderr, "caml_named_value(\"%s\") returned NULL!\n", #name); \
        exit(1);                                                        \
      }                                                                 \
    }                                                                   \
    val = CAML_CALLBACK_##argc(*name, __VA_ARGS__);                     \
  } while (0)

// We don't use the CAML macros here we don't care about the returned
// value so it's not an issue if it gets moved/collected.
#define CAML_NAMED_CALLBACK_I(name, argc, ...)            \
  do {                                                    \
    __attribute__((unused)) value bogus;                  \
    CAML_NAMED_CALLBACK(bogus, name, argc, __VA_ARGS__);  \
  } while(0)

void die_if_no_active_env()
{
  if (active_env == NULL) {
    CAML_NAMED_CALLBACK_I(no_active_env, 1, Val_unit);
    exit(1);
  }
}

/* EMACS_TO_OCAML allocates on the OCaml heap, so one must hold the OCaml runtime lock
   when using it. */
#define EMACS_TO_OCAML(val, v)                                          \
  do {                                                                  \
    die_if_no_active_env();                                             \
    val = active_env->make_global_ref(active_env, val);                 \
    v = caml_alloc_custom(&emacs_value_ops, sizeof(emacs_value), 0, 1); \
    *(emacs_value *)Data_custom_val(v) = val;                           \
    attach_async_finalizer(v);                                          \
  } while (0)

#define OCAML_TO_EMACS(v, val)                  \
  do {                                          \
    val = *(emacs_value *)Data_custom_val(v);   \
  } while(0)

void attach_async_finalizer(value val)
{
  CAMLparam1(val);
  CAML_NAMED_CALLBACK_I(attach_async_finalizer, 1, val);
  CAMLreturn0;
}

static struct custom_operations emacs_value_ops = {
  .identifier  = "emacs_value",
  .finalize    = custom_finalize_default,
  .compare     = custom_compare_default,
  .compare_ext = custom_compare_ext_default,
  .hash        = custom_hash_default,
  .serialize   = custom_serialize_default,
  .deserialize = custom_deserialize_default,
};

CAMLprim value
ecaml_have_active_env(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_bool(active_env != NULL));
}

/* [active_env->non_local_exit_get] checks a [pending_error] value in the environment that
   ends up being set by many of the [active_env] functions.  For robustness, in
   [value.ml], we follow all calls to [active_env] functions with a call to
   [ecaml_non_local_exit_get_and_clear]. */
CAMLprim value
ecaml_non_local_exit_get_and_clear(value unit)
{
  CAMLparam1(unit);
  CAMLlocal3(ret, symbol, data);

  die_if_no_active_env();

  emacs_value non_local_exit_symbol;
  emacs_value non_local_exit_data;
  enum emacs_funcall_exit non_local_exit =
    active_env->
    non_local_exit_get(active_env,
                       &non_local_exit_symbol,
                       &non_local_exit_data);

  active_env->non_local_exit_clear(active_env);

  switch (non_local_exit) {
  case emacs_funcall_exit_return:
    ret = Val_long(0);                   // Return
    break;
  case emacs_funcall_exit_signal:        // Signal (symbol, data)
    EMACS_TO_OCAML(non_local_exit_symbol, symbol);
    EMACS_TO_OCAML(non_local_exit_data, data);
    ret = caml_alloc(2, 0);
    Store_field(ret, 0, symbol);
    Store_field(ret, 1, data);
    break;
  case emacs_funcall_exit_throw:         // Throw (tag, value)
    EMACS_TO_OCAML(non_local_exit_symbol, symbol);
    EMACS_TO_OCAML(non_local_exit_data, data);
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
  die_if_no_active_env();
  emacs_value non_local_exit_symbol; OCAML_TO_EMACS(symbol, non_local_exit_symbol);
  emacs_value non_local_exit_data; OCAML_TO_EMACS(data, non_local_exit_data);
  active_env->non_local_exit_signal(active_env, non_local_exit_symbol, non_local_exit_data);
  CAMLreturn(Val_unit);
}

struct caml_cb {
  long fun_id;
};

// [Fdispatch] is the only function that we use for calling from Emacs to OCaml.
static emacs_value
Fdispatch(emacs_env *env,
          ptrdiff_t nargs,
          emacs_value args[],
          void *data)
{
  CAMLparam0();
  CAMLlocal3(arg_array, ret, tmp);

  bool need_to_lock_caml = active_env == NULL;

  emacs_env *old_env = active_env;
  active_env = env;
  if (need_to_lock_caml) {
    caml_acquire_runtime_system();
  }

  struct caml_cb *cb = (struct caml_cb *)data;

  if (nargs == 0) {
    // A zero-length array.  The OCaml manual says not to use [caml_alloc] to allocate a
    // zero-length block.
    arg_array = Atom(0);
  } else {
    arg_array = caml_alloc(nargs, 0);
    for (int i = 0; i < nargs; i++) {
      EMACS_TO_OCAML(args[i], tmp);
      Store_field(arg_array, i, tmp);
    }
  }

  CAML_NAMED_CALLBACK(ret, dispatch_function, 2, Val_long(cb->fun_id), arg_array);

  emacs_value ret_val; OCAML_TO_EMACS(ret, ret_val);

  if (need_to_lock_caml) {
    caml_release_runtime_system();
  }
  active_env = old_env;
  CAMLreturnT(emacs_value, ret_val);
}

int emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);
  assert(active_env == NULL);
  active_env = env;

  emacs_value Qargv = INTERN("argv");
  emacs_value list = FUNCALL(INTERN("symbol-value"), 1, &Qargv);
  emacs_value vect = FUNCALL(INTERN("vconcat"), 1, &list);
  int argc = active_env->vec_size(active_env, vect);
  char *argv[argc + 1];

  for (int i = 0; i < argc; i++) {
    emacs_value arg = active_env->vec_get(active_env, vect, i);
    ptrdiff_t size = 0;
    active_env->copy_string_contents(active_env, arg, NULL, &size);
    argv[i] = (char *)malloc(size * sizeof(char));
    active_env->copy_string_contents(active_env, arg, argv[i], &size);
  }

  argv[argc] = NULL;

  // [caml_startup] acquires the OCaml lock.
  caml_startup(argv);

  CAML_NAMED_CALLBACK_I(end_of_module_initialization, 1, Val_unit);

  caml_release_runtime_system();
  active_env = NULL;
  return 0;
}

void free_caml_cb(void *ptr)
{
  CAMLparam0();
  caml_acquire_runtime_system();
  struct caml_cb *cb = (struct caml_cb *)ptr;
  CAML_NAMED_CALLBACK_I(free_function, 1, Val_long(cb->fun_id));
  caml_release_runtime_system();
  free(cb);
  CAMLreturn0;
}

CAMLprim value
ecaml_make_function(value doc, value fun_id)
{
  CAMLparam2(doc, fun_id);
  CAMLlocal3(ret, ret_func, ret_uptr);
  die_if_no_active_env();
  struct caml_cb *r = (struct caml_cb *)malloc(sizeof *r);
  r->fun_id = Long_val(fun_id);
  /* [make_function] doesn't deal with NULs in OCaml strings. */
  emacs_value emacs_func =
    active_env->make_function(active_env, 0, emacs_variadic_function, Fdispatch,
                              String_val(doc), (void *)r);
  emacs_value uptr = active_env->make_user_ptr(active_env, free_caml_cb, r);
  ret = caml_alloc_tuple(2);
  EMACS_TO_OCAML(emacs_func, ret_func);
  EMACS_TO_OCAML(uptr, ret_uptr);
  Store_field(ret, 0, ret_func);
  Store_field(ret, 1, ret_uptr);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_funcall(value fun, value args)
{
  CAMLparam2(fun, args);
  CAMLlocal1(ret);
  die_if_no_active_env();
  emacs_value the_fun; OCAML_TO_EMACS(fun, the_fun);
  int nargs = Wosize_val(args);
  emacs_value the_args[nargs];
  for (int i = 0; i < nargs; i++) {
    OCAML_TO_EMACS(Field(args, i), the_args[i]);
  }
  emacs_value ret_val = FUNCALL(the_fun, nargs, the_args);
  EMACS_TO_OCAML(ret_val, ret);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_intern(value symbol_name)
{
  CAMLparam1(symbol_name);
  CAMLlocal1(ret);
  die_if_no_active_env();
  /* ok because we don't trigger CAML gc below */
  char *the_symbol_name = String_val(symbol_name);
  emacs_value ret_val = INTERN(the_symbol_name);
  EMACS_TO_OCAML(ret_val, ret);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_type_of(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  die_if_no_active_env();
  emacs_value the_val; OCAML_TO_EMACS(val, the_val);
  emacs_value ret_val = active_env->type_of(active_env, the_val);
  EMACS_TO_OCAML(ret_val, ret);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_is_not_nil(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  die_if_no_active_env();
  emacs_value the_val; OCAML_TO_EMACS(val, the_val);
  ret = Val_bool(active_env->is_not_nil(active_env, the_val));
  CAMLreturn(ret);
}

CAMLprim value
ecaml_eq(value a, value b)
{
  CAMLparam2(a, b);
  CAMLlocal1(ret);
  die_if_no_active_env();
  emacs_value the_a; OCAML_TO_EMACS(a, the_a);
  emacs_value the_b; OCAML_TO_EMACS(b, the_b);
  ret = Val_bool(active_env->eq(active_env, the_a, the_b));
  CAMLreturn(ret);
}

CAMLprim value
ecaml_to_int(value val)
{
  CAMLparam1(val);
  die_if_no_active_env();
  emacs_value the_val; OCAML_TO_EMACS(val, the_val);
  intmax_t ret_val = active_env->extract_integer(active_env, the_val);
  /* We don't check the range of [ret_val], since Emacs's [most-positive-fixnum] fits
     within OCaml's 63-bit int, and we have an expect test for that. */
  CAMLreturn(Val_long(ret_val));
}

CAMLprim value
ecaml_of_int(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  die_if_no_active_env();
  long the_val = Long_val(val);
  emacs_value ret_val = active_env->make_integer(active_env, the_val);
  EMACS_TO_OCAML(ret_val, ret);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_to_float(value val)
{
  CAMLparam1(val);
  die_if_no_active_env();
  emacs_value the_val; OCAML_TO_EMACS(val, the_val);
  double ret_val = active_env->extract_float(active_env, the_val);
  CAMLreturn(caml_copy_double(ret_val));
}

CAMLprim value
ecaml_of_float(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  die_if_no_active_env();
  double the_val = Double_val(val);
  emacs_value ret_val = active_env->make_float(active_env, the_val);
  EMACS_TO_OCAML(ret_val, ret);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_to_string(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  die_if_no_active_env();
  emacs_value the_val; OCAML_TO_EMACS(val, the_val);
  ptrdiff_t size = 0;
  active_env->copy_string_contents(active_env, the_val, NULL, &size);
  /* [copy_string_contents] writes [size] including space for a NUL byte at the end.
     Since [caml_alloc_string] allocates enough space for a NUL byte at the end anyway, we
     pass [size - 1] to [caml_alloc_string] and it is safe to allow [copy_string_contents]
     to blit into this buffer (since the last byte is NUL). */
  ret = caml_alloc_string(size - 1);
  active_env->copy_string_contents(active_env, the_val, String_val(ret), &size);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_of_string(value val)
{
  CAMLparam1(val);
  CAMLlocal1(ret);
  die_if_no_active_env();
  char *buf = String_val(val);
  int len = caml_string_length(val);
  emacs_value ret_val = active_env->make_string(active_env, buf, len);
  EMACS_TO_OCAML(ret_val, ret);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_vec_get(value vec, value index)
{
  CAMLparam2(vec, index);
  CAMLlocal1(ret);
  die_if_no_active_env();
  emacs_value the_vec; OCAML_TO_EMACS(vec, the_vec);
  long the_index = Long_val(index);
  emacs_value ret_val = active_env->vec_get(active_env, the_vec, the_index);
  EMACS_TO_OCAML(ret_val, ret);
  CAMLreturn(ret);
}

CAMLprim value
ecaml_vec_set(value vec, value index, value val)
{
  CAMLparam3(vec, index, val);
  CAMLlocal1(ret);
  die_if_no_active_env();
  emacs_value the_vec; OCAML_TO_EMACS(vec, the_vec);
  long the_index = Long_val(index);
  emacs_value the_val; OCAML_TO_EMACS(val, the_val);
  active_env->vec_set(active_env, the_vec, the_index, the_val);
  CAMLreturn(Val_unit);
}

CAMLprim value
ecaml_vec_size(value vec)
{
  CAMLparam1(vec);
  CAMLlocal1(ret);
  die_if_no_active_env();
  emacs_value the_vec; OCAML_TO_EMACS(vec, the_vec);
  ptrdiff_t ret_val = active_env->vec_size(active_env, the_vec);
  CAMLreturn(Val_long(ret_val));
}

CAMLprim value
ecaml_free_emacs_value(value val)
{
  CAMLparam1(val);
  die_if_no_active_env();
  emacs_value the_val; OCAML_TO_EMACS(val, the_val);
  active_env->free_global_ref(active_env, the_val);
  CAMLreturn(Val_unit);
}
