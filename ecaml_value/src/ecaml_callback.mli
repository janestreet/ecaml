(** This module is used to enforce the expected types of registered callbacks. *)

open! Core_kernel
open! Import

type 'a t

val register
  :  'a t
  -> Source_code_position.t
  -> f:'a
  -> should_run_holding_async_lock:bool
  -> unit

(** [dispatch_function] is how Emacs calls from C to OCaml. *)
val dispatch_function : (Caml_embedded_id.t -> Value.t array -> Value.t) t

(** The C code arranges to call [end_of_module_initialization] at the end of
    [emacs_module_init], which is what Emacs calls to initialize an Ecaml plugin.  This
    is after all the other top-level OCaml code has run. *)
val end_of_module_initialization : (unit -> unit) t

(** [free_embedded_caml_values] removes entries for any embeded ocaml values that are not
    longer used in emacs and are scheduled to be removed. The function runs whenever we
    enter OCaml from emacs, provided of course that there are values to collect. *)
val free_embedded_caml_values : (Caml_embedded_id.t array -> unit) t

(** [report_exn_when_calling_callback] is called when the infrastructure that calls a
    callbacks raises.  This is a rare situation, e.g. [Out_of_memory]. *)
val report_exn_when_calling_callback : exn -> unit
