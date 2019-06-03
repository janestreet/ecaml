open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "symbol"
    let here = [%here]
    let is_in_subtype = Value.is_symbol
  end)

let equal t1 t2 = eq t1 t2
let intern string = string |> Value.intern |> of_value_exn
let funcallN ?should_profile t args = Value.funcallN ?should_profile (t |> to_value) args

let funcallN_i ?should_profile t args =
  Value.funcallN_i ?should_profile (t |> to_value) args
;;

let funcallN_array ?should_profile t args =
  Value.funcallN_array ?should_profile (t |> to_value) args
;;

let funcallN_array_i ?should_profile t args =
  Value.funcallN_array_i ?should_profile (t |> to_value) args
;;

let funcall0 ?should_profile t = Value.funcall0 ?should_profile (t |> to_value)
let funcall1 ?should_profile t t0 = Value.funcall1 ?should_profile (t |> to_value) t0

let funcall2 ?should_profile t t0 t1 =
  Value.funcall2 ?should_profile (t |> to_value) t0 t1
;;

let funcall3 ?should_profile t t0 t1 t2 =
  Value.funcall3 ?should_profile (t |> to_value) t0 t1 t2
;;

let funcall4 ?should_profile t t0 t1 t2 t3 =
  Value.funcall4 ?should_profile (t |> to_value) t0 t1 t2 t3
;;

let funcall5 ?should_profile t t0 t1 t2 t3 t4 =
  Value.funcall5 ?should_profile (t |> to_value) t0 t1 t2 t3 t4
;;

let funcall0_i ?should_profile t = Value.funcall0_i ?should_profile (t |> to_value)
let funcall1_i ?should_profile t t0 = Value.funcall1_i ?should_profile (t |> to_value) t0

let funcall2_i ?should_profile t t0 t1 =
  Value.funcall2_i ?should_profile (t |> to_value) t0 t1
;;

let funcall3_i ?should_profile t t0 t1 t2 =
  Value.funcall3_i ?should_profile (t |> to_value) t0 t1 t2
;;

let funcall4_i ?should_profile t t0 t1 t2 t3 =
  Value.funcall4_i ?should_profile (t |> to_value) t0 t1 t2 t3
;;

let funcall5_i ?should_profile t t0 t1 t2 t3 t4 =
  Value.funcall5_i ?should_profile (t |> to_value) t0 t1 t2 t3 t4
;;

let funcall_int_int_value_value_unit ?should_profile t t0 t1 t2 t3 =
  Value.funcall_int_int_value_value_unit ?should_profile (t |> to_value) t0 t1 t2 t3
;;

let funcall_int_int_value_unit ?should_profile t t0 t1 t2 =
  Value.funcall_int_int_value_unit ?should_profile (t |> to_value) t0 t1 t2
;;

module Q = struct
  let symbol_name = "symbol-name" |> intern
end

let name t = funcall1 Q.symbol_name (t |> to_value) |> Value.to_utf8_bytes_exn
