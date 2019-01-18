open! Core_kernel
open! Async
open! Import

val initialize : unit -> unit

module Expect_test_config : module type of Expect_test_config

module Private : sig
  val block_on_async
    :  Source_code_position.t
    -> ?context:Sexp.t Lazy.t
    -> (unit -> 'a Deferred.t)
    -> 'a

  (** [run_outside_async f] schedules [f] to run at some point in the future, outside of
      Async, i.e. not during an Async cycle and without holding the Async lock.
      [run_outside_async] returns a deferred that is filled with the result of [f].  Use
      [run_outside_async] to wrap blocking Elisp functions, e.g. [read-from-minibuffer].
      For such functions, Emacs, while waiting, will run timers and network handlers, and
      hence will run Async cycles.  Because [run_outside_async] runs [f] outside of a
      Async, Emacs will be able to run cycles as needed.  Also, because [run_outside_async]
      releases the Async lock, the Async scheduler thread will be able to run and
      request cycles. *)
  val run_outside_async : (unit -> 'a) -> 'a Deferred.t
end
