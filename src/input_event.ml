open! Core_kernel
open! Import

module Q = struct
  let alt = "alt" |> Symbol.intern
  let click = "click" |> Symbol.intern
  let control = "control" |> Symbol.intern
  let double = "double" |> Symbol.intern
  let down = "down" |> Symbol.intern
  let drag = "drag" |> Symbol.intern
  let hyper = "hyper" |> Symbol.intern
  let meta = "meta" |> Symbol.intern
  let shift = "shift" |> Symbol.intern
  let super = "super" |> Symbol.intern
  let triple = "triple" |> Symbol.intern
end

module Current_buffer = Current_buffer0
module Key_sequence = Key_sequence0
include Input_event0

let read = Funcall.Wrap.("read-event" <: nullary @-> return t)

module Basic = struct
  type t =
    | Char_code of Char_code.t
    | Symbol of Symbol.t
  [@@deriving sexp_of]

  let of_value_exn value =
    if Value.is_symbol value
    then Symbol (value |> Symbol.of_value_exn)
    else (
      match Char_code.of_value_exn value with
      | char_code -> Char_code char_code
      | exception _ ->
        raise_s
          [%message
            "[Input_event.Basic.of_value_exn] got unexpected value" (value : Value.t)])
  ;;
end

let event_basic_type = Funcall.Wrap.("event-basic-type" <: t @-> return value)
let basic t = event_basic_type t |> Basic.of_value_exn

module Modifier = struct
  type t =
    | Alt
    | Click
    | Control
    | Double
    | Down
    | Drag
    | Hyper
    | Meta
    | Shift
    | Super
    | Triple
  [@@deriving enumerate, sexp_of]

  let to_symbol = function
    | Alt -> Q.alt
    | Click -> Q.click
    | Control -> Q.control
    | Double -> Q.double
    | Down -> Q.down
    | Drag -> Q.drag
    | Hyper -> Q.hyper
    | Meta -> Q.meta
    | Shift -> Q.shift
    | Super -> Q.super
    | Triple -> Q.triple
  ;;

  let of_symbol_exn =
    let assoc = List.map all ~f:(fun t -> to_symbol t, t) in
    fun symbol -> List.Assoc.find_exn assoc symbol ~equal:Symbol.equal
  ;;

  let of_value_exn value = value |> Symbol.of_value_exn |> of_symbol_exn
end

let event_modifiers = Funcall.Wrap.("event-modifiers" <: t @-> return (list value))
let modifiers t = event_modifiers t |> List.map ~f:Modifier.of_value_exn

let create_exn input =
  let key_sequence = Key_sequence.create_exn input in
  if Key_sequence.length key_sequence <> 1
  then
    raise_s
      [%message
        "[Input_event.create_exn] got key sequence not of length one"
          (input : string)
          (key_sequence : Key_sequence.t)];
  Key_sequence.get key_sequence 0
;;

let unread_command_input = Var.Wrap.("unread-command-events" <: list t)
let append = Funcall.Wrap.("append" <: value @-> value @-> return value)

let enqueue_unread_command_input ts =
  let unread_command_events = Var.Wrap.("unread-command-events" <: value) in
  Current_buffer.set_value
    unread_command_events
    (append
       (unread_command_events |> Current_buffer.value_exn)
       ((ts : t list :> Value.t list) |> Value.list))
;;

let recent_keys_internal =
  Funcall.Wrap.("recent-keys" <: bool @-> return (Vector.t Value.Type.value))
;;

let recent_keys () = recent_keys_internal false |> Array.map ~f:of_value_exn

module Command_or_key = struct
  type t =
    | Command of Command.t
    | Key of Input_event0.t
  [@@deriving sexp_of]

  let of_value_exn v =
    if Value.is_cons v
    then Command (Value.cdr_exn v |> Command.of_value_exn)
    else Key (of_value_exn v)
  ;;
end

let recent_commands_and_keys () =
  recent_keys_internal true |> Array.map ~f:Command_or_key.of_value_exn
;;
