open! Core_kernel
open! Import

type t [@@deriving sexp_of]

include Comparable.S with type t := t
include Hashable.S with type t := t
include Valueable.S with type t := t
