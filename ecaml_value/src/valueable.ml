open! Core
open! Import
include Valueable_intf

module Make (M : Type) = struct
  include M

  let of_value_exn = Value.Type.of_value_exn type_
  let to_value = Value.Type.to_value type_
  let t = type_
end
