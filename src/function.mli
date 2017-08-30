(** Functions that call from Emacs to OCaml. *)

open! Core_kernel
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
  -> Source_code_position.t
  -> args           : Symbol.t list
  -> 'a

val create : (             Fn.t -> t    ) with_spec
val defun  : ( Symbol.t -> Fn.t -> unit ) with_spec

val to_value : t -> Value.t

(** Each [defun] adds the defined function to a [list ref].  [Load_history.add_defuns]
    calls [get_and_clear_defuns] to get this [list ref] to make it possible to, within
    Emacs, jump from a symbol defined by Ecaml to the Ecaml source. *)
val get_and_clear_defuns : unit -> (Source_code_position.t * Symbol.t) list

module Expert : sig
  val raise_in_dispatch : bool ref
end
