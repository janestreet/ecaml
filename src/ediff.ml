open! Core
open! Import

let buffers =
  let buffers = Funcall.Wrap.("ediff-buffers" <: Buffer.t @-> Buffer.t @-> return nil) in
  fun ~a ~b -> Async_ecaml.Private.run_outside_async (fun () -> buffers a b)
;;
