open! Core_kernel
open! Async
open! Import

val initialize : unit -> unit

module Expect_test_config : module type of Expect_test_config_with_unit_expect

module Export : sig
  module Clock : sig
    include module type of Async.Clock

    val every'
      :  ?start:unit Deferred.t
      -> ?stop:unit Deferred.t
      -> ?continue_on_error:bool
      -> ?finished:unit Ivar.t
      -> Time.Span.t
      -> (unit -> unit Deferred.t)
      -> unit
    [@@deprecated "[since 2019-05] Use [Background.Clock.every']"]

    val every
      :  ?start:unit Deferred.t
      -> ?stop:unit Deferred.t
      -> ?continue_on_error:bool
      -> Time.Span.t
      -> (unit -> unit)
      -> unit
    [@@deprecated "[since 2019-05] Use [Background.Clock.every]"]
  end

  (** [Async.Process] is shadowed by [Ecaml.Process], so we make it available as
      [Async_process]. *)
  module Async_process = Async.Process

  (** This lets us reliably shadow Async values in [Import]. *)
  module Async = Async
  [@@deprecated "[since 2019-05] open Async before opening Import."]

  module Async_kernel = Async_kernel
  [@@deprecated "[since 2019-05] open Async_kernel before opening Import."]

  val don't_wait_for : unit Deferred.t -> unit
  [@@deprecated "[since 2019-05] Use [Background.don't_wait_for]"]
end

module Private : sig
  (** [block_on_async here f] calls [f ()], and runs Async cycles until the result is
      determined, then returns the result.

      This function cannot be called:
      - during an Async job
      - inside another call to [block_on_async]

      An exception will be raised if these conditions are violated. *)
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
      Async, Emacs will be able to run cycles as needed.  Also, because
      [run_outside_async] releases the Async lock, the Async scheduler thread will be able
      to run and request cycles. *)
  val run_outside_async
    :  Source_code_position.t
    -> ?allowed_in_background:bool (** default is [false] *)
    -> (unit -> 'a)
    -> 'a Deferred.t
end
