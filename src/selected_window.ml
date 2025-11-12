open! Core
open! Async_kernel
open! Import

let other_window = Funcall.Wrap.("other-window" <: int @-> return nil)
let height = Funcall.Wrap.("window-height" <: nullary @-> return int)
let width = Funcall.Wrap.("window-width" <: nullary @-> return int)
let get = Funcall.Wrap.("selected-window" <: nullary @-> return Window.t)
let select_window = Funcall.Wrap.("select-window" <: Window.t @-> bool @-> return nil)

let set ?(move_to_front_of_buffer_list = true) window =
  select_window window (not move_to_front_of_buffer_list)
;;

module Blocking = struct
  let pop_to_buffer =
    Funcall.Wrap.("pop-to-buffer-same-window" <: Buffer.t @-> return nil)
  ;;
end

let pop_to_buffer buffer =
  Value.Private.run_outside_async (fun () -> Blocking.pop_to_buffer buffer)
;;

let switch_to_buffer_other_window =
  let switch_to_buffer_other_window =
    Funcall.Wrap.("switch-to-buffer-other-window" <: Buffer.t @-> return nil)
  in
  fun buffer ->
    Value.Private.run_outside_async (fun () -> switch_to_buffer_other_window buffer)
;;

let split_horizontally_exn =
  Funcall.Wrap.("split-window-horizontally" <: nullary @-> return nil)
;;

let split_sensibly_exn = Funcall.Wrap.("split-window-sensibly" <: nullary @-> return nil)

let split_vertically_exn =
  Funcall.Wrap.("split-window-vertically" <: nullary @-> return nil)
;;

let quit =
  let quit = Funcall.Wrap.("quit-window" <: nullary @-> return nil) in
  fun () -> Value.Private.run_outside_async quit
;;

let save_window_excursion sync_or_async f =
  Save_wrappers.save_window_excursion sync_or_async f
;;

let save_selected_window f = Save_wrappers.save_selected_window f

let set_temporarily sync_or_async window ~f =
  Save_wrappers.with_selected_window sync_or_async (window |> Window.to_value) f
;;

let find_file =
  let find_file = Funcall.Wrap.("find-file" <: string @-> return nil) in
  fun path -> Value.Private.run_outside_async (fun () -> find_file path)
;;

let find_file_other_window =
  let find_file_other_window =
    Funcall.Wrap.("find-file-other-window" <: string @-> return nil)
  in
  fun path -> Value.Private.run_outside_async (fun () -> find_file_other_window path)
;;

let find_file_read_only =
  let find_file_read_only =
    Funcall.Wrap.("find-file-read-only" <: string @-> return nil)
  in
  fun path -> Value.Private.run_outside_async (fun () -> find_file_read_only path)
;;

let count_screen_lines =
  Funcall.Wrap.("count-screen-lines" <: Position.t @-> Position.t @-> bool @-> return int)
;;

let screen_line_at_point () =
  (* Pass COUNT-FINAL-NEWLINE=t so this returns 2 when point is at the beginning of the
     second screen line. *)
  match count_screen_lines (Window.start (get ())) (Point.get ()) true with
  | 0 | 1 ->
    0
    (* count-screen-lines returns 0 when point is at the beginning of the first line in
       the window, but 1 at other positions on that line. *)
  | n -> n - 1
;;
