(** [(Info-goto-node "(elisp)Obsolete Functions")] *)

open! Core_kernel
open! Import

(** [(describe-function 'make-obsolete)]

    Note: this does not define a function alias. See [Defun.define_obsolete_alias]. *)
val make_function_obsolete : Symbol.t -> current:Symbol.t option -> since:string -> unit

(** [(describe-function 'make-obsolete-variable)]

    Note: this does not define a variable alias. See [Defvar.define_obsolete_alias]. *)
val make_variable_obsolete : Symbol.t -> current:Symbol.t option -> since:string -> unit
