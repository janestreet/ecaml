(** [(Info-goto-node "(elisp)Compilation")] *)

open! Core_kernel
open! Import

(** [(describe-variable 'compilation-buffer-name-function)] *)
val buffer_name_function : Function.t option Var.t

(** [(describe-function 'compilation-next-error)] *)
val next_error : ?different_file:bool -> ?start_at:Position.t -> int -> unit
