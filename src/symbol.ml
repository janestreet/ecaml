open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "symbol"
    let here = [%here]
    let is_in_subtype = Value.is_symbol
  end)

let equal t1 t2 = eq t1 t2

let intern string = string |> Value.intern |> of_value_exn

module Q = struct
  let fboundp         = "fboundp"         |> intern
  let fset            = "fset"            |> intern
  let make_symbol     = "make-symbol"     |> intern
  let symbol_function = "symbol-function" |> intern
  let symbol_name     = "symbol-name"     |> intern
end

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

let name t = funcall1 Q.symbol_name (t |> to_value) |> Value.to_utf8_bytes_exn

let compare_name t1 t2 = String.compare (name t1) (name t2)

let function_is_defined t = funcall1 Q.fboundp (t |> to_value) |> Value.to_bool

let function_exn t =
  if not (function_is_defined t)
  then raise_s [%message
         "[Symbol.function_exn] of symbol with no function field"
           ~symbol:(t : t)];
  funcall1 Q.symbol_function (t |> to_value);
;;

let create ~name = funcall1 Q.make_symbol (name |> Value.of_utf8_bytes) |> of_value_exn

let set_function t value = funcall2_i Q.fset (t |> to_value) value

type symbol = t

module type Subtype = sig
  type t

  val of_symbol_exn : symbol -> t
  val to_symbol     : t      -> symbol

  val of_value_exn : Value.t -> t
  val to_value     : t       -> Value.t
end

module Make_subtype (Arg : sig
    type t [@@deriving enumerate, sexp_of]
    val module_name : string
    val to_symbol : t -> symbol
  end) = struct

  let to_symbol = Arg.to_symbol

  let of_symbol_exn =
    let assoc = List.map Arg.all ~f:(fun arg -> (to_symbol arg, arg)) in
    fun symbol ->
      match List.Assoc.find assoc symbol ~equal with
      | Some t -> t
      | None ->
        raise_s [%message
          (concat [ "[";Arg.module_name;".of_symbol] got unexpected symbol" ])
            (symbol : t)]
  ;;

  let to_value t = t |> to_symbol |> to_value

  let of_value_exn value =
    match of_value_exn value with
    | s -> s |> of_symbol_exn
    | exception _ ->
      raise_s [%message
        (concat [ "[";Arg.module_name;".of_value_exn] got unexpected value" ])
          (value : Value.t)]
  ;;
end
