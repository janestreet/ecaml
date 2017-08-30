(** Not the normal empty interface, because some slow tests are split into other
    [test_ansi_color*.ml] files. *)

open! Core_kernel
open! Import

val escape : int list -> string
val test_codes : int list -> unit
val test : string -> unit
