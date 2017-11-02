open! Core_kernel
open! Import

include Key_sequence0

let execute t = Symbol.funcall1_i Q.execute_kbd_macro (t |> to_value)

let read () ~prompt =
  Symbol.funcall1 Q.read_key_sequence_vector (prompt |> Value.of_utf8_bytes)
  |> of_value_exn
;;

let enqueue_unread_command_input t = Input_event.enqueue_unread_command_input (to_list t)
