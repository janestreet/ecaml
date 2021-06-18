open! Core
open! Import
module Current_buffer = Current_buffer0

type after_change =
  { beginning_of_changed_region : Position.t
  ; end_of_changed_region : Position.t
  ; length_before_change : int
  }
[@@deriving sexp_of]

type before_change =
  { beginning_of_changed_region : Position.t
  ; end_of_changed_region : Position.t
  }
[@@deriving sexp_of]

type file = { file : string } [@@deriving sexp_of]
type normal = unit [@@deriving sexp_of]
type frame = { frame : Frame.t } [@@deriving sexp_of]

type window =
  { window : Window.t
  ; start : Position.t
  }
[@@deriving sexp_of]

module Hook_type = struct
  type 'a t =
    | After_change_hook : after_change t
    | Before_change_hook : before_change t
    | File_hook : file t
    | Normal_hook : normal t
    | Frame_hook : frame t
    | Window_hook : window t
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

module Wrap = struct
  let ( <: ) name hook_type =
    { var = { symbol = name |> Symbol.intern; type_ = Value.Type.(list Function.t) }
    ; hook_type
    }
  ;;
end
