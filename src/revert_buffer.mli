open! Core
open! Async_kernel
open! Import

type t

(** Define a function which is suitable for use as revert-buffer-function.

    [(describe-variable 'revert-buffer-function)] *)
val define : here:[%call_pos] -> Symbol.t -> (confirm:bool -> unit Deferred.t) -> t

val lambda : here:[%call_pos] -> (confirm:bool -> unit Deferred.t) -> t

(** Like [define], but the revert function takes the buffer to revert as an explicit
    argument, so that it can be used to revert a buffer that is not current (see
    [revert_explicit_buffer_exn]). *)
val define_explicit_buffer
  :  here:[%call_pos]
  -> Symbol.t
  -> (unit, 'a) Sync_or_async.t
  -> (confirm:bool -> Buffer.t -> 'a)
  -> t

(** Set revert-buffer-function in [buffer]. *)
val set : Buffer.t -> t -> unit

(** [revert_explicit_buffer_exn buffer] reverts [buffer] without making it current, by
    calling its revert function with [buffer] as the explicit argument. Raises if
    [buffer]'s revert function was not created with [define_explicit_buffer]. It must be
    called from the foreground; a background job that wants to revert a buffer should
    hoist the revert to the foreground with
    [Background.schedule_foreground_block_on_async]. *)
val revert_explicit_buffer_exn : Buffer.t -> unit Deferred.t

module Blocking : sig
  (** [revert_explicit_buffer_exn] is [revert_explicit_buffer_exn] above, for callers that
      are already running outside of Async. *)
  val revert_explicit_buffer_exn : Buffer.t -> unit
end
