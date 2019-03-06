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
