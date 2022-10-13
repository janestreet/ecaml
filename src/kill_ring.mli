(** [(Info-goto-node "(elisp)The Kill Ring")] *)

open! Core
open! Async_kernel
open! Import

(** [(describe-variable 'kill-ring)] *)
val kill_ring : Text.t list Var.t

val is_empty : unit -> bool

(** [(describe-function 'kill-new)] *)
val kill_new : Text.t -> unit

(** [(describe-function 'current-kill)] *)
val current_kill_exn : unit -> Text.t

(** See [Current_buffer.yank] for yanking from the kill ring. *)
