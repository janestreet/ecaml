(** Functions that call from Emacs to OCaml. *)

open! Core_kernel
open! Import0

module Fn : sig
  type t = Value.t array -> Value.t
end

include Value.Subtype

type 'a with_spec
  =  ?docstring     : string
  -> ?interactive   : string
  -> ?optional_args : Symbol.t list
  -> ?rest_arg      : Symbol.t
  -> Source_code_position.t
  -> args           : Symbol.t list
  -> 'a

val create : (             Fn.t -> t    ) with_spec
val defun  : ( Symbol.t -> Fn.t -> unit ) with_spec

val to_value : t -> Value.t

module Expert : sig
  val raise_in_dispatch : bool ref
end

module For_testing : sig
  val defun_symbols : Symbol.t list ref
end
