open! Core_kernel
open! Import

module Q = struct
  include Q
  let abbrev_mode                      = "abbrev-mode"                      |> Symbol.intern
  let goto_address_mode                = "goto-address-mode"                |> Symbol.intern
  let read_only_mode                   = "read-only-mode"                   |> Symbol.intern
  let view_mode                        = "view-mode"                        |> Symbol.intern
  let visual_line_mode                 = "visual-line-mode"                 |> Symbol.intern
end


type t =
  { function_name : Symbol.t
  ; variable_name : Symbol.t }
[@@deriving sexp_of]

let abbrev =
  { function_name = Q.abbrev_mode
  ; variable_name = Q.abbrev_mode }
;;

let goto_address =
  { function_name = Q.goto_address_mode
  ; variable_name = Q.goto_address_mode }

let read_only =
  { function_name = Q.read_only_mode
  ; variable_name = Q.buffer_read_only }
;;

let view =
  { function_name = Q.view_mode
  ; variable_name = Q.view_mode }
;;

let visual_line =
  { function_name = Q.visual_line_mode
  ; variable_name = Q.visual_line_mode }

let is_enabled t =
  Current_buffer.value_exn { symbol = t.variable_name; type_ = Value.Type.bool }
;;

let disable t = Symbol.funcall1_i t.function_name (0 |> Value.of_int_exn)
let enable  t = Symbol.funcall1_i t.function_name (1 |> Value.of_int_exn)
