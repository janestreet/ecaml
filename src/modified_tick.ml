open! Core
open! Import0

include (
  Int :
  sig
    type t = int [@@deriving sexp_of]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end)

include Valueable.Make (struct
    type nonrec t = t

    let type_ = Value.Type.int
  end)

let sexp_of_t = sexp_of_opaque
