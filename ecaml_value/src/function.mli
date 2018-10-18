(** Lower-level interface for functions that call from Emacs to OCaml.

    Try [Defun] first. *)

open! Core_kernel
open! Import

module Fn : sig
  type t = Value.t array -> Value.t
end

include Value.Subtype

type 'a with_spec =
  ?docstring:string
  -> ?interactive:string
  -> ?optional_args:Symbol.t list
  -> ?rest_arg:Symbol.t
  -> Source_code_position.t
  -> args:Symbol.t list
  -> 'a

val create : (Fn.t -> t) with_spec

val create_nullary
  :  ?docstring:string
  -> ?interactive:string
  -> Source_code_position.t
  -> (unit -> unit)
  -> t

val to_value : t -> Value.t

module Expert : sig
  val raise_in_dispatch : bool ref
end
