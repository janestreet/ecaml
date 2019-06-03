open! Core_kernel
open! Import

let gcs_done = Var.create ("gcs-done" |> Symbol.intern) Value.Type.int
let gcs_done () = Current_buffer.value_exn gcs_done

let gc_elapsed =
  Var.create
    ("gc-elapsed" |> Symbol.intern)
    (Value.Type.map
       Value.Type.float
       ~name:[%sexp "time-span-as-float-seconds"]
       ~of_:Time_ns.Span.of_sec
       ~to_:Time_ns.Span.to_sec)
;;

let gc_elapsed () = Current_buffer.value_exn gc_elapsed

let garbage_collect =
  Funcall.("garbage-collect" |> Symbol.intern <: nullary @-> return nil)
;;

let post_gc_hook = Hook.create ("post-gc-hook" |> Symbol.intern) ~hook_type:Normal
