open! Core_kernel
open! Import
module Current_buffer = Current_buffer0
include Key_sequence0

let execute =
  let execute_kbd_macro = Funcall.Wrap.("execute-kbd-macro" <: t @-> return nil) in
  fun t -> Value.Private.run_outside_async [%here] (fun () -> execute_kbd_macro t)
;;

let am_executing = Var.Wrap.("executing-kbd-macro" <: bool)

let read_key_sequence_vector =
  Funcall.Wrap.("read-key-sequence-vector" <: string @-> return t)
;;

let read () ~prompt = read_key_sequence_vector prompt
let enqueue_unread_command_input t = Input_event.enqueue_unread_command_input (to_list t)

let sigusr1 =
  Value.vector [| Symbol.intern "sigusr1" |> Symbol.to_value |] |> of_value_exn
;;
