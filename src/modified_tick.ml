open! Core_kernel
open! Import

include (
  Int :
  sig
    type t = int [@@deriving sexp_of]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end)

let type_ = Value.Type.int
let t = type_
let of_value_exn = Value.Type.of_value_exn type_
let to_value = Value.Type.to_value type_
