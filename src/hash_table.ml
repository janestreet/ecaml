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

module Test = struct
  type t =
    | Eq
    | Eql
    | Equal

  let to_symbol = function
    | Eq -> Symbol.intern "eq"
    | Eql -> Symbol.intern "eql"
    | Equal -> Symbol.intern "equal"
  ;;
end

let make_hash_table =
  Funcall.Wrap.("make-hash-table" <: Symbol.t @-> Symbol.t @-> return t)
;;

let create ?(test = Test.Eql) () =
  make_hash_table (":test" |> Symbol.intern) (Test.to_symbol test)
;;

let keys = Funcall.Wrap.("hash-table-keys" <: t @-> return (list value))
let gethash = Funcall.Wrap.("gethash" <: value @-> t @-> return (nil_or value))
let find t key = gethash key t
let puthash = Funcall.Wrap.("puthash" <: value @-> value @-> t @-> return nil)
let set t ~key ~data = puthash key data t
let length = Funcall.Wrap.("hash-table-count" <: t @-> return int)
let remhash = Funcall.Wrap.("remhash" <: value @-> t @-> return nil)
let remove t key = remhash key t
