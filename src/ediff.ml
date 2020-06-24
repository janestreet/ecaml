open! Core
open! Import

module Q = struct
  include Q

  let ediff_cleanup_hook = "ediff-cleanup-hook" |> Symbol.intern
end

let buffers =
  let buffers =
    Funcall.Wrap.(
      "ediff-buffers" <: Buffer.t @-> Buffer.t @-> list Function.t @-> return nil)
  in
  fun ~a ~b ~startup_hooks ->
    let startup_hooks = List.map startup_hooks ~f:(Defun.lambda_nullary_nil [%here]) in
    Async_ecaml.Private.run_outside_async [%here] (fun () -> buffers a b startup_hooks)
;;

let cleanup_hook = Hook.create Q.ediff_cleanup_hook ~hook_type:Normal
