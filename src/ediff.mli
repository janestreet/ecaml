(** Side-by-side diffing of files or buffers.

    [(Info-goto-node "(ediff) Introduction")] *)

open! Core
open! Async_kernel
open! Import

(** [(describe-function 'ediff-buffers)] *)
val buffers : a:Buffer.t -> b:Buffer.t -> unit Deferred.t
