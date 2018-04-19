open! Core_kernel
open! Import

module Q = struct
  include Q
  let delete_window                    = "delete-window"                    |> Symbol.intern
  let set_window_buffer                = "set-window-buffer"                |> Symbol.intern
  let set_window_point                 = "set-window-point"                 |> Symbol.intern
  let window_body_height               = "window-body-height"               |> Symbol.intern
  let window_buffer                    = "window-buffer"                    |> Symbol.intern
  let window_height                    = "window-height"                    |> Symbol.intern
  let window_list                      = "window-list"                      |> Symbol.intern
  let window_live_p                    = "window-live-p"                    |> Symbol.intern
  let window_point                     = "window-point"                     |> Symbol.intern
  let window_width                     = "window-width"                     |> Symbol.intern
end

include Window0

let equal = eq

let all_in_selected_frame () =
  Symbol.funcall0 Q.window_list |> Value.to_list_exn ~f:of_value_exn
;;

let body_height_exn t =
  Symbol.funcall1 Q.window_body_height (t |> to_value) |> Value.to_int_exn
;;

let buffer_exn t = Symbol.funcall1 Q.window_buffer (t |> to_value) |> Buffer.of_value_exn
let height_exn t = Symbol.funcall1 Q.window_height (t |> to_value) |> Value.to_int_exn
let is_live    t = Symbol.funcall1 Q.window_live_p (t |> to_value) |> Value.to_bool
let point_exn  t = Symbol.funcall1 Q.window_point  (t |> to_value) |> Position.of_value_exn
let width_exn  t = Symbol.funcall1 Q.window_width  (t |> to_value) |> Value.to_int_exn

let set_buffer_exn ?(keep_margins = false) t buffer =
  Symbol.funcall3_i Q.set_window_buffer
    (t |> to_value)
    (buffer |> Buffer.to_value)
    (keep_margins |> Value.of_bool)
;;

let set_point_exn t position =
  Symbol.funcall2_i Q.set_window_point (t |> to_value) (position |> Position.to_value)
;;

let delete_exn t = Symbol.funcall1_i Q.delete_window (t |> to_value)
