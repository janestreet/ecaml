open! Core_kernel
open! Import

type t =
  { function_name : Symbol.t
  ; variable_name : Symbol.t }
[@@deriving sexp_of]

let abbrev =
  { function_name = Q.abbrev_mode
  ; variable_name = Q.abbrev_mode }
;;

let read_only =
  { function_name = Q.read_only_mode
  ; variable_name = Q.buffer_read_only }
;;

let view =
  { function_name = Q.view_mode
  ; variable_name = Q.view_mode }
;;

let is_enabled t =
  Current_buffer.value_exn { symbol = t.variable_name; type_ = Value.Type.bool }
;;

let disable t = Symbol.funcall1_i t.function_name (0 |> Value.of_int_exn)
let enable  t = Symbol.funcall1_i t.function_name (1 |> Value.of_int_exn)
