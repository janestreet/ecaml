#include <assert.h>

#include <caml/memory.h>

#include "emacs-module.h"
#include "ecaml_stubs.h"

CAMLprim value ecaml__backward_word(value a0)
{
  CAMLparam1(a0);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  static emacs_value emacs_fun = NULL;
  ecaml_cache_symbol_and_keep_alive(env, &emacs_fun, "backward-word");
  emacs_value emacs_args[1];
  emacs_args[0] = env->make_integer(env, Long_val(a0));
  emacs_value emacs_ret = env->funcall(env, emacs_fun, 1, emacs_args);
  ret = Val_bool(env->is_not_nil(env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__buffer_live_p(value a0)
{
  CAMLparam1(a0);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  static emacs_value emacs_fun = NULL;
  ecaml_cache_symbol_and_keep_alive(env, &emacs_fun, "buffer-live-p");
  emacs_value emacs_args[1];
  emacs_args[0] = emacs_of_ocaml(env, a0);
  emacs_value emacs_ret = env->funcall(env, emacs_fun, 1, emacs_args);
  ret = Val_bool(env->is_not_nil(env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__elt_returning_int(value a0, value a1)
{
  CAMLparam2(a0, a1);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  static emacs_value emacs_fun = NULL;
  ecaml_cache_symbol_and_keep_alive(env, &emacs_fun, "elt");
  emacs_value emacs_args[2];
  emacs_args[0] = emacs_of_ocaml(env, a0);
  emacs_args[1] = env->make_integer(env, Long_val(a1));
  emacs_value emacs_ret = env->funcall(env, emacs_fun, 2, emacs_args);
  ret = Val_long(env->extract_integer(env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__forward_word(value a0)
{
  CAMLparam1(a0);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  static emacs_value emacs_fun = NULL;
  ecaml_cache_symbol_and_keep_alive(env, &emacs_fun, "forward-word");
  emacs_value emacs_args[1];
  emacs_args[0] = env->make_integer(env, Long_val(a0));
  emacs_value emacs_ret = env->funcall(env, emacs_fun, 1, emacs_args);
  ret = Val_bool(env->is_not_nil(env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__point_max(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  assert(unit == Val_unit);
  static emacs_value emacs_fun = NULL;
  ecaml_cache_symbol_and_keep_alive(env, &emacs_fun, "point-max");
  emacs_value emacs_ret = env->funcall(env, emacs_fun, 0, NULL);
  ret = Val_long(env->extract_integer(env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__point_min(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(ret);
  emacs_env* env = ecaml_active_env_or_die();
  assert(unit == Val_unit);
  static emacs_value emacs_fun = NULL;
  ecaml_cache_symbol_and_keep_alive(env, &emacs_fun, "point-min");
  emacs_value emacs_ret = env->funcall(env, emacs_fun, 0, NULL);
  ret = Val_long(env->extract_integer(env, emacs_ret));
  CAMLreturn(ret);
}

