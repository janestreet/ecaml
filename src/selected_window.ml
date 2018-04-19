open! Core_kernel
open! Import

module Q = struct
  include Q
  let find_file                        = "find-file"                        |> Symbol.intern
  let select_window                    = "select-window"                    |> Symbol.intern
  let selected_window                  = "selected-window"                  |> Symbol.intern
  let split_window_horizontally        = "split-window-horizontally"        |> Symbol.intern
  let split_window_vertically          = "split-window-vertically"          |> Symbol.intern
  let switch_to_buffer                 = "switch-to-buffer"                 |> Symbol.intern
  let view_file                        = "view-file"                        |> Symbol.intern
end

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

let save_window_excursion f = Save_wrappers.save_window_excursion f
let save_selected_window  f = Save_wrappers.save_selected_window  f
