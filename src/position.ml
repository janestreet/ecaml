open! Core_kernel
open! Import

module Q = struct
  include Q

  let position_bytes = "position-bytes" |> Symbol.intern
  and byte_to_position = "byte-to-position" |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "position"
    let here = [%here]
    let is_in_subtype = Value.is_integer
  end)

module F = struct
  open Funcall

  let position_bytes = Q.position_bytes <: type_ @-> return int
  and byte_to_position = Q.byte_to_position <: int @-> return type_
end

let of_int_exn int = int |> Value.of_int_exn |> of_value_exn
let to_int t = t |> to_value |> Value.to_int_exn
let to_byte_position = F.position_bytes
let of_byte_position = F.byte_to_position
let compare t1 t2 = Int.compare (to_int t1) (to_int t2)
let add t1 t2 = of_int_exn (to_int t1 + t2)
let sub t1 t2 = of_int_exn (to_int t1 - t2)
let diff t1 t2 = to_int t1 - to_int t2

include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving compare, sexp_of]
  end)
