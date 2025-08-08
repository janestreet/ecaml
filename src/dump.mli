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

module For_testing : sig
  (** Allow further calls to [Dump] functions even after module initialization is done;
      always eval the the forms built by these calls rather than dumping them (since
      dumping isn't possible after module initialization is done).

      This is important for tests, which are dynamically loaded after module
      initialization from a separate .cmxs file. *)
  val allow_calls_after_module_initialization : unit -> unit
end
