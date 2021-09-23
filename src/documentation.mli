(** [(Info-goto-node "(elisp)Documentation")] *)

open! Core
open! Import

(** [(Info-goto-node "(elisp)Keys in Documentation")]
    [(describe-function 'substitute-command-keys)] *)
val substitute_command_keys : string -> string

(** [(Info-goto-node "(elisp)Keys in Documentation")] *)
val set_advertised_binding : Symbol.t -> Key_sequence.t -> unit

(** [(Info-goto-node "(elisp)Keys in Documentation")] *)
module Special_sequence : sig
  val command : Symbol.t -> string
  val keymap : Symbol.t -> string
end
