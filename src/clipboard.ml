open! Core_kernel
open! Async_kernel
open! Import

let () = Feature.require ("select" |> Symbol.intern)
let select_enable_clipboard = Customization.Wrap.("select-enable-clipboard" <: bool)

let with_clipboard_enabled f =
  Current_buffer.set_value_temporarily
    Sync
    (select_enable_clipboard |> Customization.var)
    true
    ~f
;;

let kill_new text = with_clipboard_enabled (fun () -> Kill_ring.kill_new text)
let yank_at_point () = with_clipboard_enabled Point.yank
