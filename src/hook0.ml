open! Core
open! Import0
module Current_buffer = Current_buffer0
module Frame = Frame0
module Window = Window0

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
  type ('a, 'b) t =
    | After_change_hook : (after_change, unit) t
    | Before_change_hook : (before_change, unit) t
    | File_hook : (file, unit) t
    | Normal_hook : (normal, unit) t
    | Frame_hook : (frame, unit) t
    | Window_hook : (window, unit) t
    | Query_function : (normal, bool) t
  [@@deriving sexp_of]
end

type ('a, 'b) t =
  { var : Function.t list Var.t
  ; hook_type : ('a, 'b) Hook_type.t
  }
[@@deriving fields ~getters]

let symbol t = t.var.symbol
let value t = Current_buffer.value t.var
let value_exn t = Current_buffer.value_exn t.var

let sexp_of_t _ _ t =
  [%message
    ""
      ~symbol:(symbol t : Symbol.t)
      ~hook_type:(t.hook_type : (_, _) Hook_type.t)
      ~value:(value t : Function.t list option)]
;;

module Wrap = struct
  let ( <: ) name hook_type =
    { var = { symbol = name |> Symbol.intern; type_ = Value.Type.(list Function.t) }
    ; hook_type
    }
  ;;
end
