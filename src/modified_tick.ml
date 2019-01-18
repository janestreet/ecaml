open! Core_kernel
open! Import

include (
  Int :
  sig
    type t = int [@@deriving sexp_of]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end)

let ({ Value.Type.of_value_exn; to_value; id = _ } as type_) = Value.Type.int
