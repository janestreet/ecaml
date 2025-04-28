open! Core
open! Import

let buffers =
  let buffers =
    Funcall.Wrap.(
      "ediff-buffers" <: Buffer.t @-> Buffer.t @-> list Function.t @-> return nil)
  in
  fun ~a ~b ~startup_hooks ->
    let startup_hooks = List.map startup_hooks ~f:(Defun.lambda_nullary_nil [%here]) in
    Async_ecaml.Private.run_outside_async (fun () -> buffers a b startup_hooks)
;;

let cleanup_hook = Hook.Wrap.("ediff-cleanup-hook" <: Normal_hook)
