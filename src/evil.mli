(** Functions for interacting with evil-mode *)

open! Core
open! Import

module Config : sig
  type one =
    | Ignore_repeat
    (** [(describe-function 'evil-declare-ignore-repeat)]

        If a command is declared non-repeatable, [evil-repeat] will ignore that command
        instead of adding it to [evil-repeat-ring]. *)

  type t = one list

  val apply_to_defun : t -> Symbol.t -> unit
end

module Escape : sig
  (** [(describe-variable 'evil-escape-inhibit-functions)] *)
  val inhibit_functions : Function.t list Var.t
end

module State : sig
  type t =
    | Evilified
    | Normal
    | Other of Symbol.t
  [@@deriving equal, sexp_of]

  (** [(describe-variable 'evil-state)] *)
  val get : unit -> t

  (** [(describe-function 'evil-insert)] *)
  val insert : unit -> unit
end

val is_in_use : unit -> bool

(** [(describe-function 'evil-define-key* )] *)
val define_key : State.t list -> Keymap.t -> Key_sequence.t -> Keymap.Entry.t -> unit

(** Updates evil-mode's visual selection variables if mark and point have been moved
    programmatically. Normally this is already called by [evil-visual-post-command-hook],
    but it may be useful for code that sets mark outside of a command.

    This function may be called unconditionally when mark is moved; it checks whether
    [evil-mode] is active in the current buffer and if the current evil state is [visual].

    [(describe-function 'evil-visual-refresh)] *)
val visual_refresh_if_necessary : unit -> unit
