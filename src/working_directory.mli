open! Core_kernel
open! Import

type t =
  | Of_current_buffer
  | Root
  | This of string
[@@deriving sexp_of]

val within : t -> f:(unit -> 'a) -> 'a
