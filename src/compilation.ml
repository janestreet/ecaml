open! Core_kernel
open! Import

module Q = struct
  include Q
  let compilation_buffer_name_function = "compilation-buffer-name-function" |> Symbol.intern
end

let buffer_name_function =
  Var.create Q.compilation_buffer_name_function (Value.Type.option Function.type_)
;;
