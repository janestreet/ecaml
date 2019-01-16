open! Core_kernel
open! Import

(** Define constants.  (N.B. Elisp constants are mutable.  Read the docs.) *)

(** [(describe-function 'defconst)] *)
val defconst
  :  Symbol.t
  -> Source_code_position.t
  -> docstring:string
  -> 'a Value.Type.t
  -> 'a
  -> 'a Var.t

val defconst_i
  :  Symbol.t
  -> Source_code_position.t
  -> docstring:string
  -> 'a Value.Type.t
  -> 'a
  -> unit
