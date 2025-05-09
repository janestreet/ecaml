open! Core
open! Import
module Keymap := Keymap0

val eval_and_dump : here:Source_code_position.t -> (unit -> Form.t) -> unit

(** Call [Defun.defalias] and also dump a "declare-function" for this symbol. *)
val defalias : here:Source_code_position.t -> Symbol.t -> Value.t -> unit

val keymap_set
  :  here:Source_code_position.t
  -> Keymap.t Var.t
  -> (string * Symbol.t) list
  -> unit
