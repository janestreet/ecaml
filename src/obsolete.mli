(** [(Info-goto-node "(elisp)Obsolete Functions")] *)

open! Core_kernel
open! Import

(** [(describe-function 'make-obsolete)] *)
val make : ?when_:string -> Symbol.t -> current:Symbol.t -> unit

(** [(describe-function 'define-obsolete-function-alias)]

    N.B. Load order matters.  A subsequent [defun] will override the aliasing.
*)
val alias : ?docstring:string -> ?when_:string -> Symbol.t -> current:Symbol.t -> unit
