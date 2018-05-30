open! Core_kernel
open! Import

(** [(Info-goto-node "(elisp)Obsolete Functions")] *)

(** [(describe-function 'make-obsolete)] *)
val make : ?when_:string -> Symbol.t -> current:Symbol.t -> unit

(** [(describe-function 'define-obsolete-function-alias)]

    N.B. Load order matters.  A subsequent [defun] will override the aliasing.
*)
val alias : ?docstring:string -> ?when_:string -> Symbol.t -> current:Symbol.t -> unit
