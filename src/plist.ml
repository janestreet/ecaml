open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "plist"
    let here = [%here]

    let rec is_in_subtype value =
      Value.is_nil value
      || (Value.is_cons value
          && Value.is_symbol (Value.car_exn value)
          &&
          let cdr = Value.cdr_exn value in
          Value.is_cons cdr && is_in_subtype (Value.cdr_exn cdr))
    ;;
  end)

module Q = struct
  let plist_get = "plist-get" |> Symbol.intern
  and plist_put = "plist-put" |> Symbol.intern
  and symbol_plist = "symbol-plist" |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let plist_get = Q.plist_get <: type_ @-> Symbol.type_ @-> return (option value)
  let plist_put = Q.plist_put <: type_ @-> Symbol.type_ @-> value @-> return nil
  let symbol_plist = Q.symbol_plist <: Symbol.type_ @-> return type_
end

let get = F.plist_get
let set = F.plist_put
let of_symbol = F.symbol_plist
