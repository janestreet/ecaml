(** [(Info-goto-node "(elisp)Garbage Collection")] *)

open! Core_kernel
open! Import

(** [(describe-function 'garbage-collect)] *)
val garbage_collect : unit -> unit

(** [(describe-variable 'gcs-done)] *)
val gcs_done : unit -> int

(** [(describe-variable 'gc-elapsed)] *)
val gc_elapsed : unit -> Time_ns.Span.t

(** [(describe-variable 'post-gc-hook)] *)
val post_gc_hook : Hook.normal Hook.t
