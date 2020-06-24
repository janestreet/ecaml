open! Core_kernel
open! Import

let gcs_done = Var.Wrap.("gcs-done" <: int)
let gcs_done () = Current_buffer.value_exn gcs_done

let gc_elapsed =
  Var.Wrap.(
    "gc-elapsed"
    <: Value.Type.map
         float
         ~name:[%sexp "time-span-as-float-seconds"]
         ~of_:Time_ns.Span.of_sec
         ~to_:Time_ns.Span.to_sec)
;;

let gc_elapsed () = Current_buffer.value_exn gc_elapsed
let garbage_collect = Funcall.Wrap.("garbage-collect" <: nullary @-> return nil)
let post_gc_hook = Hook.create ("post-gc-hook" |> Symbol.intern) ~hook_type:Normal
