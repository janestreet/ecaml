open! Core_kernel
open! Import

module Q = struct
  include Q
  let hash_table_keys                  = "hash-table-keys"                  |> Symbol.intern
  let make_hash_table                  = "make-hash-table"                  |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "hash-table"
    let here = [%here]
    let is_in_subtype = Value.is_hash_table
  end)

module F = struct
  open Funcall
  open Value.Type

  let hash_table_keys = Q.hash_table_keys <: type_ @-> return (list string)

  let make_hash_table = Q.make_hash_table <: nullary @-> return type_
end

let create = F.make_hash_table

let keys = F.hash_table_keys
