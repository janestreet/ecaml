(** [(Info-goto-node "(elisp)Compilation")] *)

open! Core
open! Import

val major_mode : Major_mode.t

(** [(describe-variable 'compilation-buffer-name-function)] *)
val buffer_name_function : Function.t option Var.t

(** [(describe-function 'compilation-find-buffer)] *)
val find_buffer : ?avoid_current:bool -> unit -> Buffer.t

(** [(describe-function 'compilation-next-error)] *)
val next_error : ?different_file:bool -> ?start_at:Position.t -> int -> unit
