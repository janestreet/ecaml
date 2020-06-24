open! Core_kernel
open! Import0

include (
  Int :
  sig
    type t = int [@@deriving sexp_of]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end)

include (val Valueable.of_type Value.Type.int)
