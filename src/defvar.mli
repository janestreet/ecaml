open! Core_kernel
open! Import

(** [(describe-function 'defvar)]
    [(Info-goto-node "(elisp)Defining Variables")] *)
val defvar : Source_code_position.t -> Symbol.t -> Value.t -> docstring:string -> unit
