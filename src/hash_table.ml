open! Core_kernel
open! Import

let () =
  (* For: [hash-table-empty-p], [hash-table-keys], [hash-table-values]. *)
  Feature.require ("subr-x" |> Symbol.intern)
;;

include Value.Make_subtype (struct
    let name = "hash-table"
    let here = [%here]
    let is_in_subtype = Value.is_hash_table
  end)

let create = Funcall.("make-hash-table" <: nullary @-> return t)
let keys = Funcall.("hash-table-keys" <: t @-> return (list string))
