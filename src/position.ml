open! Core
open! Import0

include Value.Make_subtype (struct
    let name = "position"
    let here = [%here]
    let is_in_subtype = Value.is_integer
  end)

let of_int_exn int = int |> Value.of_int_exn |> of_value_exn
let to_int t = t |> to_value |> Value.to_int_exn

let to_byte_position_exn =
  let position_bytes = Funcall.Wrap.("position-bytes" <: t @-> return (nil_or int)) in
  fun pos ->
    match position_bytes pos with
    | Some bpos -> bpos
    | None -> raise_s [%message "Position out of range" (pos : t)]
;;

let of_byte_position_exn =
  let byte_to_position = Funcall.Wrap.("byte-to-position" <: int @-> return (nil_or t)) in
  fun bpos ->
    match byte_to_position bpos with
    | Some pos -> pos
    | None -> raise_s [%message "Byte position out of range" (bpos : int)]
;;

let compare t1 t2 = Int.compare (to_int t1) (to_int t2)
let add t1 t2 = of_int_exn (to_int t1 + t2)
let sub t1 t2 = of_int_exn (to_int t1 - t2)
let diff t1 t2 = to_int t1 - to_int t2

include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving compare, sexp_of]
  end)
