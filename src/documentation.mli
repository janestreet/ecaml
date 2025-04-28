(** [(Info-goto-node "(elisp)Documentation")] *)

open! Core
open! Import

(** [(Info-goto-node "(elisp)Keys in Documentation")]
    [(describe-function 'substitute-command-keys)] *)
val substitute_command_keys : Text.t -> Text.t

(** [(Info-goto-node "(elisp)Keys in Documentation")] *)
val set_advertised_binding : Symbol.t -> Key_sequence.t -> unit

(** [(Info-goto-node "(elisp)Keys in Documentation")] *)
module Special_sequence : sig
  (** A key sequence that will invoke the command, or "M-x command" if the command does
      not have a key binding. *)
  val command : Symbol.t -> string

  (** A link to the documentation for a symbol (usually a function or variable). *)
  val symbol : Symbol.t -> string

  (** A link to the documentation for a customizable variable. *)
  val customization : _ Customization.t -> string

  (** [describe_keymap VAR] generates a summary of a keymap stored in [VAR], using
      [(describe-function 'describe-bindings)]. *)
  val describe_keymap : Symbol.t -> string

  (** Assume the named keymap is active, for any following {!command} sequences in the
      current documentation string. (This does not generate any text on its own.) *)
  val assume_keymap : Symbol.t -> string
end

(** Convenience module for constructing documentation strings with substitution sequences.

    [O] is designed to be locally opened, like so:

    {[
      let () =
        message_text
          Documentation.O.(
            substitute_command_keys
              [%string {| Press %{command Q.foo} to invoke %{symbol Q.foo}. |}])
      ;;
    ]} *)
module O : sig
  val substitute_command_keys : string -> Text.t

  include module type of Special_sequence
end
