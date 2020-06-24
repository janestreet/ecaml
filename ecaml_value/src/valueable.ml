open! Core_kernel
open! Import
include Valueable_intf

let of_type (type a) (type_ : a Value.Type.t) : (module S with type t = a) =
  (module struct
    type t = a

    let of_value_exn = Value.Type.of_value_exn type_
    let to_value = Value.Type.to_value type_
    let t = type_
    let type_ = type_
  end)
;;
