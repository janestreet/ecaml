(** Lower-level interface for functions that call from Emacs to OCaml.

    Try [Defun] first. *)

open! Core_kernel
open! Import

module Fn : sig
  type t = Value.t array -> Value.t
end

include Value.Subtype

val create
  :  Source_code_position.t
  -> ?docstring:string
  -> ?interactive:Value.t
  -> args:Symbol.t list
  -> ?optional_args:Symbol.t list
  -> ?rest_arg:Symbol.t
  -> Fn.t
  -> t

val create_nullary
  :  Source_code_position.t
  -> ?docstring:string
  -> ?interactive:Value.t
  -> (unit -> unit)
  -> t

val to_value : t -> Value.t
val of_symbol : Symbol.t -> t

module Expert : sig
  val raise_in_dispatch : bool ref
end
