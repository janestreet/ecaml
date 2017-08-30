CAMLprim value ecaml__backward_word(value a0)
{
  CAMLparam1(a0);
  CAMLlocal1(ret);
  static emacs_value emacs_fun = NULL;
  cache_symbol_and_keep_alive(&emacs_fun, "backward-word");
  emacs_value emacs_args[1];
  emacs_args[0] = active_env->make_integer(active_env, Long_val(a0));
  emacs_value emacs_ret = funcall(emacs_fun, 1, emacs_args);
  ret = Val_bool(active_env->is_not_nil(active_env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__buffer_live_p(value a0)
{
  CAMLparam1(a0);
  CAMLlocal1(ret);
  static emacs_value emacs_fun = NULL;
  cache_symbol_and_keep_alive(&emacs_fun, "buffer-live-p");
  emacs_value emacs_args[1];
  emacs_args[0] = EMACS_OF_OCAML(a0);
  emacs_value emacs_ret = funcall(emacs_fun, 1, emacs_args);
  ret = Val_bool(active_env->is_not_nil(active_env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__elt_returning_int(value a0, value a1)
{
  CAMLparam2(a0, a1);
  CAMLlocal1(ret);
  static emacs_value emacs_fun = NULL;
  cache_symbol_and_keep_alive(&emacs_fun, "elt");
  emacs_value emacs_args[2];
  emacs_args[0] = EMACS_OF_OCAML(a0);
  emacs_args[1] = active_env->make_integer(active_env, Long_val(a1));
  emacs_value emacs_ret = funcall(emacs_fun, 2, emacs_args);
  ret = Val_long(active_env->extract_integer(active_env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__forward_word(value a0)
{
  CAMLparam1(a0);
  CAMLlocal1(ret);
  static emacs_value emacs_fun = NULL;
  cache_symbol_and_keep_alive(&emacs_fun, "forward-word");
  emacs_value emacs_args[1];
  emacs_args[0] = active_env->make_integer(active_env, Long_val(a0));
  emacs_value emacs_ret = funcall(emacs_fun, 1, emacs_args);
  ret = Val_bool(active_env->is_not_nil(active_env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__point_max(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(ret);
  assert(unit == Val_unit);
  static emacs_value emacs_fun = NULL;
  cache_symbol_and_keep_alive(&emacs_fun, "point-max");
  emacs_value emacs_ret = funcall(emacs_fun, 0, NULL);
  ret = Val_long(active_env->extract_integer(active_env, emacs_ret));
  CAMLreturn(ret);
}

CAMLprim value ecaml__point_min(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(ret);
  assert(unit == Val_unit);
  static emacs_value emacs_fun = NULL;
  cache_symbol_and_keep_alive(&emacs_fun, "point-min");
  emacs_value emacs_ret = funcall(emacs_fun, 0, NULL);
  ret = Val_long(active_env->extract_integer(active_env, emacs_ret));
  CAMLreturn(ret);
}

