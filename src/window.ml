open! Core_kernel
open! Async_kernel
open! Import
include Window0

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

module Blocking = struct
  let set_buffer_exn =
    let set_window_buffer =
      Funcall.("set-window-buffer" <: t @-> Buffer.t @-> bool @-> return nil)
    in
    fun ?(keep_margins = false) t buffer -> set_window_buffer t buffer keep_margins
  ;;
end

let set_buffer_exn ?keep_margins t buffer =
  Value.Private.run_outside_async [%here] (fun () ->
    Blocking.set_buffer_exn ?keep_margins t buffer)
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
