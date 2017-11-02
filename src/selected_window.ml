open! Core_kernel
open! Import

let get () = Symbol.funcall0 Q.selected_window |> Window.of_value_exn

let set
      ?(move_to_front_of_buffer_list = true)
      window
  =
  Symbol.funcall2_i Q.select_window
    (window |> Window.to_value)
    (not move_to_front_of_buffer_list |> Value.of_bool)
;;

let switch_to_buffer buffer =
  Symbol.funcall1_i Q.switch_to_buffer (buffer |> Buffer.to_value)
;;

let split_horizontally_exn () = Symbol.funcall0_i Q.split_window_horizontally
let split_vertically_exn   () = Symbol.funcall0_i Q.split_window_vertically

let find_file path = Symbol.funcall1_i Q.find_file (path |> Value.of_utf8_bytes)
let view_file path = Symbol.funcall1_i Q.view_file (path |> Value.of_utf8_bytes)
