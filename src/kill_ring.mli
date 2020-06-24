(** [(Info-goto-node "(elisp)The Kill Ring")] *)

open! Core_kernel
open! Async_kernel
open! Import

(** [(describe-variable 'kill-ring)] *)
val kill_ring : Text.t list Var.t

val is_empty : unit -> bool

(** [(describe-function 'kill-new)] *)
val kill_new : Text.t -> unit

(** See [Current_buffer.yank] for yanking from the kill ring. *)
