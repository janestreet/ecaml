(** A background job is an Async job that the emacs UI is not blocking on. These jobs are
    special -- they can only interact with the UI in limited ways to avoid interfering
    with blocking jobs, or with the user. For example, background jobs may not change the
    current buffer. *)

open! Core_kernel
open! Async
open! Import0

(** [don't_wait_for here f] is like [Async.don't_wait_for], except that it marks all
    jobs created by running [f] as background jobs. *)
val don't_wait_for : Source_code_position.t -> (unit -> unit Deferred.t) -> unit

(** From a background job, become a foreground job.  We implement this by waiting until no
    other foreground jobs are running, then blocking emacs until the [Deferred.t] is
    resolved. *)
val schedule_foreground_block_on_async
  :  Source_code_position.t
  -> (unit -> unit Deferred.t)
  -> unit

module Clock : sig
  (** [every'] is like [Async.Clock.every'], except that it marks
      the repeating jobs as background jobs. *)
  val every'
    :  ?start:unit Deferred.t
    -> ?stop:unit Deferred.t
    -> ?continue_on_error:bool
    -> ?finished:unit Ivar.t
    -> Source_code_position.t
    -> Time.Span.t
    -> (unit -> unit Deferred.t)
    -> unit

  (** [every] is like [Async.Clock.every], except that it marks
      the repeating jobs as background jobs. *)
  val every
    :  ?start:unit Deferred.t
    -> ?stop:unit Deferred.t
    -> ?continue_on_error:bool
    -> Source_code_position.t
    -> Time.Span.t
    -> (unit -> unit)
    -> unit
end

(** If this is called in a background Async job, raise an exception. *)
val assert_foreground : ?message:Sexp.t -> Source_code_position.t -> unit

(** Return [Some location] when the current Async job is a background job. [location] is
    the [Source_code_position.t] describing where the job was originally tagged as running
    in the background. *)
val currently_running_in_background : unit -> Source_code_position.t option

val am_running_in_background : unit -> bool
val am_running_in_foreground : unit -> bool

module Private : sig
  val mark_running_in_background : Source_code_position.t -> f:(unit -> 'a) -> 'a
  val mark_running_in_foreground : f:(unit -> 'a) -> 'a

  val schedule_foreground_block_on_async
    :  Source_code_position.t
    -> ?raise_exceptions_to_monitor:Monitor.t (** for testing, default: Monitor.main *)
    -> (unit -> unit Deferred.t)
    -> unit
end
