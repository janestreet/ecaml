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

let get = Funcall.Wrap.("plist-get" <: t @-> Symbol.t @-> return (nil_or value))
let set = Funcall.Wrap.("plist-put" <: t @-> Symbol.t @-> value @-> return nil)
let of_symbol = Funcall.Wrap.("symbol-plist" <: Symbol.t @-> return t)
