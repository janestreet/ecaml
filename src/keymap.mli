(** The command bindings of input events are recorded in data structures called keymaps.
    Each entry in a keymap associates (or "binds") an individual event type, either to
    another keymap or to a command.  When an event type is bound to a keymap, that keymap
    is used to look up the next input event; this continues until a command is found.  The
    whole process is called "key lookup".

    [(Info-goto-node "(elisp)Keymaps")] *)

open! Core_kernel
open! Import
include Value.Subtype

type keymap := t

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
val create : ?kind:Kind.t (** default is [Sparse] *) -> ?menu_name:string -> unit -> t

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
val set_transient : ?keep_if_used:bool (** default is [false] *) -> t -> unit

module Entry : sig
  type t =
    | Absent
    | Command of Command.t
    | Keyboard_macro of Key_sequence.t
    | Keymap of keymap
    | Symbol of Symbol.t
    | Undefined (** [(describe-function 'undefined)] *)
    | Value of Value.t
  [@@deriving sexp_of]

  include Valueable.S with type t := t
end

(** [(describe-function 'lookup-key)]
    [(Info-goto-node "(elisp)Functions for Key Lookup")] *)
val lookup_key_exn
  :  ?accept_defaults:bool (** default is [false] *)
  -> t
  -> Key_sequence.t
  -> Entry.t

(** [(describe-function 'define-key)]
    [(Info-goto-node "(elisp)Functions for Key Lookup")] *)
val define_key : t -> Key_sequence.t -> Entry.t -> unit

(** [(describe-variable 'minor-mode-map-alist)] *)
val minor_mode_map_alist : (Symbol.t * t) list Var.t

(** [override_minor_mode_map minor_mode ~f] overrides the bindings for [minor_mode] in the
    current buffer with the result of calling [f] on the keymap for [minor_mode].

    If no such map exists, an empty sparse map is created.

    [(describe-variable 'minor-mode-map-alist)]
    [(describe-variable 'minor-mode-overriding-map-alist)]
    [(Info-goto-node "(elisp)Controlling Active Maps")] *)
val override_minor_mode_map : Symbol.t -> f:(t -> unit) -> unit

(** [(describe-variable 'special-event-map)] *)
val special_event_map : t Var.t

(** [(describe-function 'suppress-keymap)] *)
val suppress_keymap : ?suppress_digits:bool (** default is [false] *) -> t -> unit
