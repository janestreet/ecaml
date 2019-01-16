(** [(Info-goto-node "(elisp)Obsolete Functions")] *)

open! Core_kernel
open! Import

(** [(describe-function 'make-obsolete)] *)
val make_function_obsolete : Symbol.t -> current:Symbol.t -> since:string -> unit

(** [(describe-function 'make-obsolete-variable)] *)
val make_variable_obsolete : Symbol.t -> current:Symbol.t -> since:string -> unit
