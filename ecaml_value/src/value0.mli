(** This module exists to break the dependency between [Value] and [Callback]. *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

val sexp_of_t_ref : (t -> Sexp.t) ref
