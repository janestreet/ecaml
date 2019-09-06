open! Core
open! Import

module Q = struct
  include Q

  let ediff_cleanup_hook = "ediff-cleanup-hook" |> Symbol.intern
end

let buffers =
  Funcall.("ediff-buffers" <: Buffer.t @-> Buffer.t @-> list Function.t @-> return nil)
;;

let buffers ~a ~b ~startup_hooks =
  let startup_hooks = List.map startup_hooks ~f:(Defun.lambda_nullary_nil [%here]) in
  buffers a b startup_hooks
;;

let cleanup_hook = Hook.create Q.ediff_cleanup_hook ~hook_type:Normal
