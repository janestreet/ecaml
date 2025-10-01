(** Functions in [Env] modify both the Emacs environment and the Unix environment. *)

open! Core
open! Async
open! Import

val putenv : key:string -> data:string -> unit
val unsetenv : string -> unit
val getenv : string -> string option
val getenv_exn : string -> string
