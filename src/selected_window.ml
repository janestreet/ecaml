open! Core_kernel
open! Import

module Q = struct
  include Q

  let find_file = "find-file" |> Symbol.intern
  and find_file_other_window = "find-file-other-window" |> Symbol.intern
  and other_window = "other-window" |> Symbol.intern
  and quit_window = "quit-window" |> Symbol.intern
  and select_window = "select-window" |> Symbol.intern
  and selected_window = "selected-window" |> Symbol.intern
  and split_window_horizontally = "split-window-horizontally" |> Symbol.intern
  and split_window_sensibly = "split-window-sensibly" |> Symbol.intern
  and split_window_vertically = "split-window-vertically" |> Symbol.intern
  and switch_to_buffer = "switch-to-buffer" |> Symbol.intern
  and switch_to_buffer_other_window = "switch-to-buffer-other-window" |> Symbol.intern
  and view_file = "view-file" |> Symbol.intern
end

module F = struct
  open Funcall

  let find_file_other_window = Q.find_file_other_window <: string @-> return nil
  let other_window = Q.other_window <: int @-> return nil
  let quit_window = Q.quit_window <: nullary @-> return nil
  let split_window_sensibly = Q.split_window_sensibly <: nullary @-> return nil
end

let get () = Symbol.funcall0 Q.selected_window |> Window.of_value_exn

let set ?(move_to_front_of_buffer_list = true) window =
  Symbol.funcall2_i
    Q.select_window
    (window |> Window.to_value)
    (not move_to_front_of_buffer_list |> Value.of_bool)
;;

let switch_to_buffer buffer =
  Symbol.funcall1_i Q.switch_to_buffer (buffer |> Buffer.to_value)
;;

let switch_to_buffer_other_window buffer =
  Symbol.funcall1_i Q.switch_to_buffer_other_window (buffer |> Buffer.to_value)
;;

let split_horizontally_exn () = Symbol.funcall0_i Q.split_window_horizontally
let split_sensibly_exn = F.split_window_sensibly
let split_vertically_exn () = Symbol.funcall0_i Q.split_window_vertically
let find_file path = Symbol.funcall1_i Q.find_file (path |> Value.of_utf8_bytes)
let find_file_other_window path = F.find_file_other_window path
let view_file path = Symbol.funcall1_i Q.view_file (path |> Value.of_utf8_bytes)
let quit = F.quit_window
let save_window_excursion f = Save_wrappers.save_window_excursion f
let save_selected_window f = Save_wrappers.save_selected_window f
let other_window = F.other_window

let set_temporarily window ~f =
  Save_wrappers.with_selected_window (window |> Window.to_value) f
;;
