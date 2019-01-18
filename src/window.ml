open! Core_kernel
open! Import

module Q = struct
  include Q

  let delete_other_windows = "delete-other-windows" |> Symbol.intern
  and delete_window = "delete-window" |> Symbol.intern
  and get_buffer_window = "get-buffer-window" |> Symbol.intern
  and never = "never" |> Symbol.intern
  and set_window_buffer = "set-window-buffer" |> Symbol.intern
  and set_window_margins = "set-window-margins" |> Symbol.intern
  and set_window_point = "set-window-point" |> Symbol.intern
  and set_window_start = "set-window-start" |> Symbol.intern
  and window_body_height = "window-body-height" |> Symbol.intern
  and window_buffer = "window-buffer" |> Symbol.intern
  and window_end = "window-end" |> Symbol.intern
  and window_frame = "window-frame" |> Symbol.intern
  and window_height = "window-height" |> Symbol.intern
  and window_list = "window-list" |> Symbol.intern
  and window_live_p = "window-live-p" |> Symbol.intern
  and window_point = "window-point" |> Symbol.intern
  and window_start = "window-start" |> Symbol.intern
  and window_width = "window-width" |> Symbol.intern
end

include Window0

module Include_minibuffer = struct
  module T = struct
    type t =
      | Yes
      | No
      | Only_if_active
    [@@deriving enumerate, sexp_of]
  end

  include T

  let never = Q.never |> Symbol.to_value

  let type_ =
    Value.Type.enum
      [%sexp "include-minibuffer"]
      (module T)
      (function
        | Yes -> Value.t
        | No -> never
        | Only_if_active -> Value.nil)
  ;;
end

module F = struct
  open! Funcall
  open! Value.Type

  let delete_other_windows = Q.delete_other_windows <: option type_ @-> return nil
  and delete_window = Q.delete_window <: option type_ @-> return nil
  and get_buffer_window = Q.get_buffer_window <: Buffer.type_ @-> return type_
  and set_window_margins =
    Q.set_window_margins <: type_ @-> option int @-> option int @-> return nil
  and set_window_start = Q.set_window_start <: type_ @-> Position.type_ @-> return nil
  and window_frame = Q.window_frame <: type_ @-> return Frame.type_

  and window_list =
    Q.window_list
    <: option Frame.type_
       @-> option Include_minibuffer.type_
       @-> option type_
       @-> return (list type_)

  and window_start = Q.window_start <: type_ @-> return Position.type_
  and window_end = Q.window_end <: type_ @-> Value.Type.bool @-> return Position.type_
end

let equal = eq

let all_in_selected_frame ?include_minibuffer () =
  F.window_list None include_minibuffer None
;;

let body_height_exn t =
  Symbol.funcall1 Q.window_body_height (t |> to_value) |> Value.to_int_exn
;;

let buffer_exn t = Symbol.funcall1 Q.window_buffer (t |> to_value) |> Buffer.of_value_exn
let get_buffer_window buffer = F.get_buffer_window buffer
let height_exn t = Symbol.funcall1 Q.window_height (t |> to_value) |> Value.to_int_exn
let is_live t = Symbol.funcall1 Q.window_live_p (t |> to_value) |> Value.to_bool
let point_exn t = Symbol.funcall1 Q.window_point (t |> to_value) |> Position.of_value_exn
let width_exn t = Symbol.funcall1 Q.window_width (t |> to_value) |> Value.to_int_exn

let set_buffer_exn ?(keep_margins = false) t buffer =
  Symbol.funcall3_i
    Q.set_window_buffer
    (t |> to_value)
    (buffer |> Buffer.to_value)
    (keep_margins |> Value.of_bool)
;;

let set_point_exn t position =
  Symbol.funcall2_i Q.set_window_point (t |> to_value) (position |> Position.to_value)
;;

let set_window_margins ?left_margin ?right_margin window =
  F.set_window_margins window left_margin right_margin
;;

let delete_exn t = F.delete_window (Some t)
let delete_other_windows = F.delete_other_windows
let start t = F.window_start t
let set_start t position = F.set_window_start t position
let end_ ?(update = false) t = F.window_end t update
let frame t = F.window_frame t
