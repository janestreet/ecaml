open! Core_kernel
open! Import

module Current_buffer = Current_buffer0
module Key_sequence   = Key_sequence0

include Input_event0

let read () = Symbol.funcall0 Q.read_event |> of_value_exn

module Basic = struct
  type t =
    | Char_code of Char_code.t
    | Symbol    of Symbol.t
  [@@deriving sexp_of]

  let of_value_exn value =
    if Value.is_symbol value
    then Symbol (value |> Symbol.of_value_exn)
    else (
      match Char_code.of_value_exn value with
      | char_code -> Char_code char_code
      | exception _ ->
        raise_s [%message
          "[Input_event.Basic.of_value_exn] got unexpected value" (value : Value.t)])
  ;;
end

let basic t = Symbol.funcall1 Q.event_basic_type (t |> to_value) |> Basic.of_value_exn

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
    | Alt     -> Q.alt
    | Click   -> Q.click
    | Control -> Q.control
    | Double  -> Q.double
    | Down    -> Q.down
    | Drag    -> Q.drag
    | Hyper   -> Q.hyper
    | Meta    -> Q.meta
    | Shift   -> Q.shift
    | Super   -> Q.super
    | Triple  -> Q.triple
  ;;

  let of_symbol_exn =
    let assoc = List.map all ~f:(fun t -> (to_symbol t, t)) in
    fun symbol ->
      List.Assoc.find_exn assoc symbol ~equal:Symbol.equal
  ;;

  let of_value_exn value = value |> Symbol.of_value_exn |> of_symbol_exn
end

let modifiers t =
  Symbol.funcall1 Q.event_modifiers (t |> to_value)
  |> Value.to_list_exn ~f:Modifier.of_value_exn
;;

let create_exn input =
  let key_sequence = Key_sequence.create_exn input in
  if Key_sequence.length key_sequence <> 1
  then raise_s [%message
         "[Input_event.create_exn] got key sequence not of length one"
           (input : string)
           (key_sequence : Key_sequence.t)];
  Key_sequence.get key_sequence 0
;;

let unread_command_input = Var.create Q.unread_command_events Value.Type.(list type_)

let enqueue_unread_command_input ts =
  let unread_command_events = Var.create Q.unread_command_events Value.Type.value in
  Current_buffer.set_value unread_command_events
    (Symbol.funcall2 Q.append
       (unread_command_events |> Current_buffer.value_exn)
       ((ts : t list :> Value.t list) |> Value.list))
;;
