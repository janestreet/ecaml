(** This module is used to enforce the expected types of registered callbacks. *)

open! Core
open! Async
open! Import

module Value = Value0

type 'a t

val register
  :  'a t
  -> f : 'a
  -> should_run_holding_async_lock : bool
  -> unit

(** [attach_async_finalizer] is called whenever a [Value.t] is created, to attach an Async
    finalizer to the value that will, when the OCaml value is freed, free the Emacs value.
    The finalizer is an Async finalizer, not an ordinary finalizer, to ensure that it runs
    in an Async from an Emacs callback. *)
val attach_async_finalizer : (Value.t -> unit) t

(** [dispatch_function] is how Emacs calls from C to OCaml. *)
val dispatch_function : (Function_id.t -> Value.t array -> Value.t) t

(** The C code arranges to call [end_of_module_initialization] at the end of
    [emacs_module_init], which is what Emacs calls to initialize an Ecaml plugin.  This
    is after all the other top-level OCaml code has run. *)
val end_of_module_initialization : (unit -> unit) t

(** [free_function function_id] is called by Emacs when it garbage collects the sentinel
    associated with the callback whose id is [function_id].  This callback does not have
    an active env, and so is not allowed to call Emacs from OCaml. *)
val free_function : (Function_id.t -> unit) t

(** [no_active_env] is used when the C code detects that OCaml is attempting to call an
    Emacs function but there is no active env.  It prints a message that includes an
    OCaml backtrace, which may be useful in debugging. *)
val no_active_env : (unit -> unit) t
