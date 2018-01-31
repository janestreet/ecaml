(** The command bindings of input events are recorded in data structures called keymaps.
    Each entry in a keymap associates (or "binds") an individual event type, either to
    another keymap or to a command.  When an event type is bound to a keymap, that keymap
    is used to look up the next input event; this continues until a command is found.  The
    whole process is called "key lookup".

    [(Info-goto-node "(elisp)Keymaps")] *)

open! Core_kernel
open! Import

include Value.Subtype

type keymap = t

include Equal.S with type t := t

(** [(describe-function 'keymap-parent)]
    [(Info-goto-node "(elisp)Inheritance and Keymaps")] *)
val parent : t -> t option

(** [(describe-function 'set-keymap-parent)]
    [(Info-goto-node "(elisp)Inheritance and Keymaps")] *)
val set_parent : t -> t option -> unit

module Kind : sig
  type t =
    | Full
    | Sparse
  [@@deriving sexp_of]
end

(** [(describe-function 'make-sparse-keymap)]
    [(describe-function 'make-keymap)]
    [(Info-goto-node "(elisp) Creating Keymaps")] *)
val create
  :  ?kind      : Kind.t  (** default is [Sparse] *)
  -> ?menu_name : string
  -> unit
  -> t

(** [(describe-function 'copy-keymap)]
    [(Info-goto-node "(elisp) Creating Keymaps")] *)
val deep_copy : t -> t

(** [(describe-function 'current-global-map)]
    [(Info-goto-node "(elisp)Controlling Active Maps")] *)
val global : unit -> t

(** [(describe-function 'use-global-map)]
    [(Info-goto-node "(elisp)Controlling Active Maps")] *)
val set_global : t -> unit

(** [(describe-function 'set-transient-map)] *)
val set_transient : t -> unit

module Entry : sig
  type t =
    | Absent
    | Command        of  Command.t
    | Keyboard_macro of  Key_sequence.t
    | Keymap         of  keymap
    | Symbol         of  Symbol.t
    | Undefined                          (** [(describe-function 'undefined)] *)
    | Value          of  Value.t
  [@@deriving sexp_of]

  include Valueable.S with type t := t
end

(** [(describe-function 'lookup-key)]
    [(Info-goto-node "(elisp)Functions for Key Lookup")] *)
val lookup_key_exn
  :  ?accept_defaults : bool  (** default is [false] *)
  -> t
  -> Key_sequence.t
  -> Entry.t

(** [(describe-function 'define-key)]
    [(Info-goto-node "(elisp)Functions for Key Lookup")] *)
val define_key : t -> Key_sequence.t -> Entry.t -> unit
