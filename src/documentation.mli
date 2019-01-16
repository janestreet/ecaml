(** [(Info-goto-node "(elisp)Documentation")] *)

open! Core_kernel
open! Import

(** [(Info-goto-node "(elisp)Keys in Documentation")]
    [(describe-function 'substitute-command-keys)] *)
val substitute_command_keys : string -> string

(** [(Info-goto-node "(elisp)Keys in Documentation")] *)
module Special_sequence : sig
  val keymap : Symbol.t -> string
end
