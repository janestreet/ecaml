#ifndef ECAML_STUBS_H
#define ECAML_STUBS_H

/* [ecaml_active_env_or_die] find the active environment */
emacs_env* ecaml_active_env_or_die();

/* [ecaml_cache_symbol_and_keep_alive] interns the given string as an
   emacs symbol, wraps it as a ocaml value and caches it. */
emacs_value ecaml_cache_symbol_and_keep_alive(emacs_env *env, emacs_value *cache, char* symbol);

/* This is [ecaml_cache_symbol_and_keep_alive(_, "t")]. */
emacs_value ecaml_emacs_t();

emacs_value emacs_of_ocaml(emacs_env* env, value val);

#endif /* ECAML_STUBS_H */
