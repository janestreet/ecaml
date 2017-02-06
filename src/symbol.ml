open! Core
open! Async
open! Import

module T : sig
  type t [@@deriving sexp_of]
  val of_value_exn : Value.t -> t
  val to_value     : t -> Value.t
end = struct
  type t = Value.t [@@deriving sexp_of]

  let to_value = Fn.id

  let of_value_exn value =
    if not (Value.is_symbol value)
    then raise_s [%message "[Symbol.of_value_exn] got non-symbol"
                             (value : Value.t)];
    value
  ;;
end

include T

let intern string = string |> Value.intern |> of_value_exn

let apply       = intern "apply"
let cons        = intern "cons"
let eval        = intern "eval"
let format      = intern "format"
let fset        = intern "fset"
let interactive = intern "interactive"
let lambda      = intern "lambda"
let list        = intern "list"
let nil         = intern "nil"
let progn       = intern "progn"
let provide     = intern "provide"
let require     = intern "require"
let quote       = intern "quote"
let read        = intern "read"
let set         = intern "set"
let symbol_name = intern "symbol-name"
let t           = intern "t"

let funcall   t args = Value.funcall   (t |> to_value) args
let funcall_i t args = Value.funcall_i (t |> to_value) args

let fset t value = funcall_i fset [ t |> to_value; value ]
let set  t value = funcall_i set  [ t |> to_value; value ]

let provide t = funcall_i provide [ t |> to_value ]

let require t = funcall_i require [ t |> to_value ]

let name t = funcall symbol_name [ t |> to_value ] |> Value.to_string
