open! Core_kernel
open! Async
open! Import

val initialize : unit -> unit

(** Run the Async scheduler. Does nothing if the scheduler is already running. *)
val start_scheduler : unit -> unit

module Expect_test_config : module type of Expect_test_config
