open! Core_kernel
open! Import0

include Value.Make_subtype (struct
    let name = "symbol"
    let here = [%here]
    let is_in_subtype = Value.is_symbol
  end)

let equal t1 t2 = eq t1 t2

let intern string = string |> Value.intern |> of_value_exn

let funcallN   t args = Value.funcallN   (t |> to_value) args
let funcallN_i t args = Value.funcallN_i (t |> to_value) args

let funcallN_array   t args = Value.funcallN_array   (t |> to_value) args
let funcallN_array_i t args = Value.funcallN_array_i (t |> to_value) args

let funcall0 t                = Value.funcall0 (t |> to_value)
let funcall1 t t0             = Value.funcall1 (t |> to_value) t0
let funcall2 t t0 t1          = Value.funcall2 (t |> to_value) t0 t1
let funcall3 t t0 t1 t2       = Value.funcall3 (t |> to_value) t0 t1 t2
let funcall4 t t0 t1 t2 t3    = Value.funcall4 (t |> to_value) t0 t1 t2 t3
let funcall5 t t0 t1 t2 t3 t4 = Value.funcall5 (t |> to_value) t0 t1 t2 t3 t4

let funcall0_i t                = Value.funcall0_i (t |> to_value)
let funcall1_i t t0             = Value.funcall1_i (t |> to_value) t0
let funcall2_i t t0 t1          = Value.funcall2_i (t |> to_value) t0 t1
let funcall3_i t t0 t1 t2       = Value.funcall3_i (t |> to_value) t0 t1 t2
let funcall4_i t t0 t1 t2 t3    = Value.funcall4_i (t |> to_value) t0 t1 t2 t3
let funcall5_i t t0 t1 t2 t3 t4 = Value.funcall5_i (t |> to_value) t0 t1 t2 t3 t4

let funcall_int_int_value_value_unit t t0 t1 t2 t3 =
  Value.funcall_int_int_value_value_unit (t |> to_value) t0 t1 t2 t3
;;

let funcall_int_int_value_unit t t0 t1 t2 =
  Value.funcall_int_int_value_unit (t |> to_value) t0 t1 t2
;;
