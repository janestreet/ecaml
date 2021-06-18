(** View the current emacs stack. *)

open! Core
open! Import

(** [(describe-function 'backtrace)] *)
val get : unit -> string
