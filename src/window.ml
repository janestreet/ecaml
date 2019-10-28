open! Core_kernel
open! Import

module Q = struct
  include Q

  let never = "never" |> Symbol.intern
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

  let t = type_
end

let window_list =
  Funcall.(
    "window-list"
    <: nil_or Frame.t @-> nil_or Include_minibuffer.t @-> nil_or t @-> return (list t))
;;

let all_in_selected_frame ?include_minibuffer () =
  window_list None include_minibuffer None
;;

let body_height_exn = Funcall.("window-body-height" <: t @-> return int)
let buffer = Funcall.("window-buffer" <: t @-> return (nil_or Buffer.t))

let buffer_exn t =
  match buffer t with
  | Some x -> x
  | None -> raise_s [%message "Window.buffer_exn" ~_:(t : t)]
;;

let get_buffer_window = Funcall.("get-buffer-window" <: Buffer.t @-> return t)
let height_exn = Funcall.("window-height" <: t @-> return int)
let is_live = Funcall.("window-live-p" <: t @-> return bool)
let point_exn = Funcall.("window-point" <: t @-> return Position.t)
let width_exn = Funcall.("window-width" <: t @-> return int)

let set_window_buffer =
  Funcall.("set-window-buffer" <: t @-> Buffer.t @-> bool @-> return nil)
;;

let set_buffer_exn ?(keep_margins = false) t buffer =
  set_window_buffer t buffer keep_margins
;;

let set_point_exn = Funcall.("set-window-point" <: t @-> Position.t @-> return nil)

let set_window_margins =
  Funcall.("set-window-margins" <: t @-> nil_or int @-> nil_or int @-> return nil)
;;

let set_window_margins ?left_margin ?right_margin window =
  set_window_margins window left_margin right_margin
;;

let delete_window = Funcall.("delete-window" <: nil_or t @-> return nil)
let delete_exn t = delete_window (Some t)
let delete_other_windows = Funcall.("delete-other-windows" <: nil_or t @-> return nil)
let start = Funcall.("window-start" <: t @-> return Position.t)
let set_start = Funcall.("set-window-start" <: t @-> Position.t @-> return nil)
let window_end = Funcall.("window-end" <: t @-> Value.Type.bool @-> return Position.t)
let end_ ?(update = false) t = window_end t update
let frame = Funcall.("window-frame" <: t @-> return Frame.t)

let fit_window_to_buffer =
  Funcall.(
    "fit-window-to-buffer"
    <: t
       @-> nil_or int
       @-> nil_or int
       @-> nil_or int
       @-> nil_or int
       @-> nil_or bool
       @-> return nil)
;;

let fit_to_buffer ?max_columns ?max_lines ?min_columns ?min_lines ?preserve_size t =
  fit_window_to_buffer t max_lines min_lines max_columns min_columns preserve_size
;;
