open! Core
open! Import

type t =
  { line : int
  ; column : int
  }
[@@deriving sexp_of]
