(** Functions that call from Emacs to OCaml. *)

open! Core
open! Async
open! Import

module Fn : sig
  type t = Value.t array -> Value.t
end

type t [@@deriving sexp_of]

type 'a with_spec
  =  ?docstring     : string
  -> ?interactive   : string
  -> ?optional_args : Symbol.t list
  -> ?rest_arg      : Symbol.t
  -> args           : Symbol.t list
  -> 'a

val create : (             Fn.t -> t    ) with_spec
val defun  : ( Symbol.t -> Fn.t -> unit ) with_spec

val to_value : t -> Value.t
