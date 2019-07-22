open! Core_kernel
open! Import
module Current_buffer = Current_buffer0

type file = { file : string } [@@deriving sexp_of]
type normal = unit [@@deriving sexp_of]

type window =
  { window : Window.t
  ; start : Position.t
  }
[@@deriving sexp_of]

module Hook_type = struct
  type 'a t =
    | File : file t
    | Normal : normal t
    | Window : window t
  [@@deriving sexp_of]
end

type 'a t =
  { var : Function.t list Var.t
  ; hook_type : 'a Hook_type.t
  }
[@@deriving fields]

let symbol t = t.var.symbol
let value t = Current_buffer.value t.var
let value_exn t = Current_buffer.value_exn t.var

let sexp_of_t _ t =
  [%message
    ""
      ~symbol:(symbol t : Symbol.t)
      ~hook_type:(t.hook_type : _ Hook_type.t)
      ~value:(value t : Function.t list option)]
;;

let create symbol ~hook_type =
  { var = { symbol; type_ = Value.Type.(list Function.t) }; hook_type }
;;
