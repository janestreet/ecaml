open! Core_kernel
open! Import

let buffer_name_function =
  Var.create Q.compilation_buffer_name_function (Value.Type.option Function.type_)
;;
