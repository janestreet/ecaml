open! Core
open! Async
open! Import
module Expect_test_config : Expect_test_config_types.S with type 'a IO.t = 'a Deferred.t

module Expect_test_config_allowing_nested_block_on_async :
  Expect_test_config_types.S with type 'a IO.t = 'a Deferred.t

module Export : sig
  module Clock : sig
    include module type of Async.Clock

    val every'
      :  ?start:unit Deferred.t
      -> ?stop:unit Deferred.t
      -> ?continue_on_error:bool
      -> ?finished:unit Ivar.t
      -> Time_float.Span.t
      -> (unit -> unit Deferred.t)
      -> unit
    [@@alert use_background_instead "Use [Background.Clock.every'] instead"]

    val every
      :  ?start:unit Deferred.t
      -> ?stop:unit Deferred.t
      -> ?continue_on_error:bool
      -> Time_float.Span.t
      -> (unit -> unit)
      -> unit
    [@@alert use_background_instead "Use [Background.Clock.every] instead"]
  end

  module Clock_ns : sig
    include module type of Async.Clock_ns

    val every'
      :  ?start:unit Deferred.t
      -> ?stop:unit Deferred.t
      -> ?continue_on_error:bool
      -> ?finished:unit Ivar.t
      -> Time_ns.Span.t
      -> (unit -> unit Deferred.t)
      -> unit
    [@@alert use_background_instead "Use [Background.Clock.every'] instead"]

    val every
      :  ?start:unit Deferred.t
      -> ?stop:unit Deferred.t
      -> ?continue_on_error:bool
      -> Time_ns.Span.t
      -> (unit -> unit)
      -> unit
    [@@alert use_background_instead "Use [Background.Clock.every] instead"]
  end

  (** [Async.Process] is shadowed by [Ecaml.Process], so we make it available as
      [Async_process]. *)
  module Async_process = Async.Process

  (** This lets us reliably shadow Async values in [Import]. *)
  module Async = Async
  [@@alert open_before_import "open Async before opening Import"]

  module Async_kernel = Async_kernel
  [@@alert open_before_import "open Async_kernel before opening Import"]

  val don't_wait_for : unit Deferred.t -> unit
  [@@alert use_background_instead "Use [Background.don't_wait_for] instead"]
end

module Private : sig
  (** [block_on_async here f] calls [f ()], and runs Async cycles until the result is
      determined, then returns the result.

      This function cannot be called:
      - during an Async job
      - inside another call to [block_on_async]

      An exception will be raised if these conditions are violated. *)
  val block_on_async
    :  here:[%call_pos]
    -> ?context:Sexp.t Lazy.t
    -> ?for_testing_allow_nested_block_on_async:bool
    -> (unit -> 'a Deferred.t)
    -> 'a

  (** [enqueue_foreground_block_on_async here f] is like [block_on_async here f], except
      that it does not run immediately, and it can be run from inside another
      [block_on_async] or an Async job. Async_ecaml will run [f] at some point, when no
      other [block_on_async] is running. *)
  val enqueue_foreground_block_on_async
    :  here:[%call_pos]
    -> ?context:Sexp.t Lazy.t
    -> ?raise_exceptions_to_monitor:Monitor.t
    -> (unit -> unit Deferred.t)
    -> unit

  (** [run_outside_async f] is the idiomatic way to call functions [f] where [f] only
      wraps a single call into Elisp, and [f] would otherwise raise
      {[
        "Called [block_on_async] in the middle of an Async job!"
      ]}
      .

      [f] should do nothing but make the one call into elisp, unless you can show that the
      work done in OCaml is safe to run with pre-emptive multi-threading semantics.

      [run_outside_async f] schedules [f] to run at some point in the future, outside of
      Async, i.e. not during an Async cycle and without holding the Async lock.

      This is necessary so that Emacs will run timers and network handlers while waiting
      for [f], and hence will request and run the Async cycles needed by [f]. *)
  val run_outside_async
    :  here:[%call_pos]
    -> ?allowed_in_background:bool (** default is [false] *)
    -> (unit -> 'a)
    -> 'a Deferred.t

  (** [run_outside_asyncN] are convenience wrappers around [run_outside_async] for common
      function arities. Feel free to add more as necessary. *)

  val run_outside_async1
    :  here:[%call_pos]
    -> ?allowed_in_background:bool (** default is [false] *)
    -> ('a -> 'r)
    -> 'a
    -> 'r Deferred.t
end
