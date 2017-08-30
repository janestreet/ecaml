open! Core_kernel
open! Import

let provide t = Symbol.funcall1_i Q.provide (t |> Symbol.to_value)

let require t = Symbol.funcall1_i Q.require (t |> Symbol.to_value)

let is_provided t =
  Symbol.funcall1 Q.featurep (t |> Symbol.to_value) |> Value.to_bool
;;

let all_provided () =
  Symbol.value_exn Q.features |> Value.to_list_exn ~f:Symbol.of_value_exn
;;
