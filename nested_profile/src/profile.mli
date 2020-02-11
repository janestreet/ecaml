(** This is a library for time profiling sequential or Async code.

    Wrap interesting places in your code with calls to [profile]:

    {[
      open! Nested_profile;;

      profile "do_thing" (fun () ->
        profile "foo" foo;
        profile "bar" bar;
        ...)
    ]}

    Then each top-level call to [profile] will output a hierarchical display of all
    profile calls beneath it:

    {v
      1.284s -- do_thing
        209.266ms -- foo
        1.074s -- bar
    v} *)

open! Core_kernel
open! Async_kernel

(** A [Sync_or_async.t] specifies whether the computation being profiled is
    synchronous or asynchronous. *)
module Sync_or_async : sig
  type _ t =
    | Sync : _ t
    | Async : _ Deferred.t t
  [@@deriving sexp_of]
end

(** [profile sync_or_async message f] measures the time taken to run [f], and outputs the
    result.  Nested calls to profile are displayed as a tree.  Use [Async] to profile
    Async code.  It is safe to nest calls to [profile] inside calls to [profile Async].
    [profile Async] does have some restrictions in order to get valid results:
    - a nested call to [profile Async] must exit before its parent does
    - there cannot be multiple sibling calls to [profile Async] running concurrently *)
val profile
  :  ?hide_if_less_than:Time_ns.Span.t
  -> 'a Sync_or_async.t
  -> Sexp.t Lazy.t
  -> (unit -> 'a)
  -> 'a

(** [am_forcing_message ()] returns true when profiling is calling [force] on a message
    supplied to [profile].  This is useful if you want to have a [sexp_of_t] function
    behave differently when producing messages. *)
val am_forcing_message : unit -> bool

val approximate_line_length_limit : int ref
val should_profile : bool ref
val hide_if_less_than : Time_ns.Span.t ref
val hide_top_level_if_less_than : Time_ns.Span.t ref
val sexp_of_time_ns : (Time_ns.t -> Sexp.t) ref
val tag_frames_with : (unit -> Sexp.t option) option ref

(** [profile] calls [output_profile] to display the profile to the user.  It is
    [print_string] by default. *)
val output_profile : (string -> unit) ref

module Start_location : sig
  type t =
    | Line_preceding_profile
    | End_of_profile_first_line
  [@@deriving compare, enumerate, sexp_of]
end

(** [start_location] specifies where in a profile the start time is rendered. *)
val start_location : Start_location.t ref

(** Return the current stack of [profile] messages, sorted inner profile frames first.
    Returns [None] if [should_profile] is false. *)
val backtrace : unit -> Sexp.t list option

(** [disown f] runs [f] as though it were at top level, rather than whatever the current
    context is.  In other words, it causes the parent (current) context to disown children
    created by [f].  This is useful for background jobs that might outlive their parents,
    since Nested_profile requires that parents wait for their children. *)
val disown : (unit -> 'a) -> 'a

module Private : sig
  module Clock : sig
    type t [@@deriving sexp_of]

    val create : now:Time_ns.t -> t
    val advance : t -> by:Time_ns.Span.t -> unit
    val now : t -> Time_ns.t
  end

  val record_frame : start:Time_ns.t -> stop:Time_ns.t -> message:Sexp.t Lazy.t -> unit
  val clock : Clock.t ref
  val on_async_out_of_order : (Sexp.t Lazy.t -> unit) ref
end
