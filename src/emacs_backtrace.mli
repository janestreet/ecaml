(** View the current emacs stack. *)

open! Core_kernel
open! Import

(** [(describe-function 'backtrace)] *)
val get : unit -> string
