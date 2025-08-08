(** Functions for interacting with evil-mode *)

open! Core
open! Import

module Escape : sig
  (** [(describe-variable 'evil-escape-inhibit-functions)] *)
  val inhibit_functions : Function.t list Var.t
end

module State : sig
  type t =
    | Evilified
    | Motion
    | Normal
    | Other of Symbol.t
  [@@deriving equal, sexp_of]

  (** [(describe-function 'evil-normal-state-p)] *)
  val is_active : t -> bool

  (** [(describe-function 'evil-normal-state)] *)
  val activate : t -> unit

  (** [(describe-function 'evil-insert)] *)
  val insert : unit -> unit
end

(** [(describe-variable 'evil-local-mode)] *)
val is_in_use : unit -> bool

(** [(describe-variable 'evil-mode)] *)
val is_in_use_globally : unit -> bool

(** [(describe-function 'evil-define-key* )] *)
val define_key : State.t list -> Keymap.t -> Key_sequence.t -> Keymap.Entry.t -> unit

(** Updates evil-mode's visual selection variables if mark and point have been moved
    programmatically. Normally this is already called by [evil-visual-post-command-hook],
    but it may be useful for code that sets mark outside of a command.

    This function may be called unconditionally when mark is moved; it checks whether
    [evil-local-mode] is active in the current buffer and if the current evil state is
    [visual].

    [(describe-function 'evil-visual-refresh)] *)
val visual_refresh_if_necessary : unit -> unit
