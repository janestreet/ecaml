open! Core_kernel
open! Import

type t =
  { line : int
  ; column : int
  }
[@@deriving sexp_of]
