(** Define constants. (N.B. Elisp constants are mutable. Read the docs.)

    [(Info-goto-node "(elisp)Defining Variables")] *)

open! Core
open! Import

(** [(describe-function 'defconst)] *)
val defconst
  :  Symbol.t
  -> Source_code_position.t
  -> docstring:string
  -> type_:'a Value.Type.t
  -> value:'a
  -> 'a Var.t

val defconst_i
  :  Symbol.t
  -> Source_code_position.t
  -> docstring:string
  -> type_:'a Value.Type.t
  -> value:'a
  -> unit
