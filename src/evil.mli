(** Functions for interacting with evil-mode *)

open! Core_kernel
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
    | Other of Symbol.t
  [@@deriving equal, sexp_of]

  (** [(describe-variable 'evil-state)] *)
  val get : unit -> t

  (** [(describe-function 'evil-insert)] *)
  val insert : unit -> unit
end

val is_in_use : unit -> bool
