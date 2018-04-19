open! Core_kernel
open! Import

module Q = struct
  include Q
  let featurep                         = "featurep"                         |> Symbol.intern
  let features                         = "features"                         |> Symbol.intern
end

include Feature0

let provide t = Symbol.funcall1_i Q.provide (t |> Symbol.to_value)

let is_provided t =
  Symbol.funcall1 Q.featurep (t |> Symbol.to_value) |> Value.to_bool
;;

let features = Var.create Q.features (Value.Type.list Symbol.type_)

let all_provided () = Current_buffer.value_exn features
