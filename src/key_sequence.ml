open! Core_kernel
open! Import
module Current_buffer = Current_buffer0

module Q = struct
  include Key_sequence0.Q

  let execute_kbd_macro = "execute-kbd-macro" |> Symbol.intern
  and executing_kbd_macro = "executing-kbd-macro" |> Symbol.intern
  and read_key_sequence_vector = "read-key-sequence-vector" |> Symbol.intern
end

include (
  Key_sequence0 :
    module type of struct
    include Key_sequence0
  end
  with module Q := Key_sequence0.Q)

let execute t = Symbol.funcall1_i Q.execute_kbd_macro (t |> to_value)
let am_executing = Var.create Q.executing_kbd_macro Value.Type.bool

let read () ~prompt =
  Symbol.funcall1 Q.read_key_sequence_vector (prompt |> Value.of_utf8_bytes)
  |> of_value_exn
;;

let enqueue_unread_command_input t = Input_event.enqueue_unread_command_input (to_list t)

let sigusr1 =
  Value.vector [| Symbol.intern "sigusr1" |> Symbol.to_value |] |> of_value_exn
;;
