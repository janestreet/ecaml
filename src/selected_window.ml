open! Core_kernel
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
  let switch_to_buffer = Funcall.Wrap.("switch-to-buffer" <: Buffer.t @-> return nil)
end

let switch_to_buffer buffer =
  Value.Private.run_outside_async [%here] (fun () -> Blocking.switch_to_buffer buffer)
;;

let switch_to_buffer_other_window =
  let switch_to_buffer_other_window =
    Funcall.Wrap.("switch-to-buffer-other-window" <: Buffer.t @-> return nil)
  in
  fun buffer ->
    Value.Private.run_outside_async [%here] (fun () ->
      switch_to_buffer_other_window buffer)
;;

let split_horizontally_exn =
  Funcall.Wrap.("split-window-horizontally" <: nullary @-> return nil)
;;

let split_sensibly_exn = Funcall.Wrap.("split-window-sensibly" <: nullary @-> return nil)

let split_vertically_exn =
  Funcall.Wrap.("split-window-vertically" <: nullary @-> return nil)
;;

let find_file_other_window =
  Funcall.Wrap.("find-file-other-window" <: string @-> return nil)
;;

let quit =
  let quit = Funcall.Wrap.("quit-window" <: nullary @-> return nil) in
  fun () -> Value.Private.run_outside_async [%here] quit
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
  fun path -> Value.Private.run_outside_async [%here] (fun () -> find_file path)
;;

let view_file =
  let view_file = Funcall.Wrap.("view-file" <: string @-> return nil) in
  fun path -> Value.Private.run_outside_async [%here] (fun () -> view_file path)
;;
