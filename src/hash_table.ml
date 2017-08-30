open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "hash-table"
    let here = [%here]
    let is_in_subtype = Value.is_hash_table
  end)

let create () = Symbol.funcall0 Q.make_hash_table |> of_value_exn
