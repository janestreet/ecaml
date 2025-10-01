(** Lower-level interface for functions that call from Emacs to OCaml.

    Try [Defun] first. *)

open! Core
open! Import
open! Async_kernel

module Fn : sig
  type t = Value.t array -> Value.t
end

include Value.Subtype

val of_ocaml_func0 : Source_code_position.t -> (unit -> Value.t) -> t
val of_ocaml_func1 : Source_code_position.t -> (Value.t -> Value.t) -> t
val of_ocaml_func2 : Source_code_position.t -> (Value.t -> Value.t -> Value.t) -> t

val of_ocaml_funcN_M
  :  Source_code_position.t
  -> min_args:int
  -> max_args:int option
  -> (Value0.t array -> Value0.t)
  -> t

val of_symbol_exn : Symbol.t -> t
val of_symbol_unsafe : Symbol.t -> t
