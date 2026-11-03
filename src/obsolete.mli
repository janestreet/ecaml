(** [(Info-goto-node "(elisp)Obsolete Functions")] *)

open! Core
open! Import

(** [(describe-function 'make-obsolete)]

    Note: this does not define a function alias. See [Defun]'s [?obsoletes] argument. *)
val make_function_obsolete : Symbol.t -> current:Symbol.t option -> since:string -> unit

(** [(describe-function 'make-obsolete-variable)]

    Note: this does not define a variable alias. See [Defvar.defvaralias]. *)
val make_variable_obsolete : Symbol.t -> current:Symbol.t option -> since:string -> unit
