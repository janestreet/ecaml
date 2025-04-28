open! Core
open! Import
include Key_sequence0

let execute =
  let execute_kbd_macro = Funcall.Wrap.("execute-kbd-macro" <: t @-> return nil) in
  fun t -> Value.Private.run_outside_async (fun () -> execute_kbd_macro t)
;;

let am_executing = Var.Wrap.("executing-kbd-macro" <: bool)

let read_key_sequence_vector =
  Funcall.Wrap.("read-key-sequence-vector" <: string @-> return t)
;;

let read () ~prompt = read_key_sequence_vector prompt
let enqueue_unread_command_input t = Input_event.enqueue_unread_command_input (to_list t)

let of_list =
  Array.of_list >> Array.map ~f:Input_event.to_value >> Value.vector >> of_value_exn
;;

let sigusr1 =
  of_list [ Symbol.intern "sigusr1" |> Symbol.to_value |> Input_event.of_value_exn ]
;;

let concat = List.concat_map ~f:to_list >> of_list
let invoking_this_command = Funcall.Wrap.("this-command-keys" <: nullary @-> return t)
