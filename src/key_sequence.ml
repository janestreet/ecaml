open! Core_kernel
open! Import

module Q = struct
  include Key_sequence0.Q
  let execute_kbd_macro                = "execute-kbd-macro"                |> Symbol.intern
  let read_key_sequence_vector         = "read-key-sequence-vector"         |> Symbol.intern
end

include (Key_sequence0 : module type of struct include Key_sequence0 end with module Q := Q)

let execute t = Symbol.funcall1_i Q.execute_kbd_macro (t |> to_value)

let read () ~prompt =
  Symbol.funcall1 Q.read_key_sequence_vector (prompt |> Value.of_utf8_bytes)
  |> of_value_exn
;;

let enqueue_unread_command_input t = Input_event.enqueue_unread_command_input (to_list t)
