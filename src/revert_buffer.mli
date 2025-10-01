open! Core
open! Async_kernel
open! Import

type t

(** Define a function which is suitable for use as revert-buffer-function.

    [(describe-variable 'revert-buffer-function)] *)
val define : here:[%call_pos] -> Symbol.t -> (confirm:bool -> unit Deferred.t) -> t

val lambda : here:[%call_pos] -> (confirm:bool -> unit Deferred.t) -> t

val lambda_background_safe
  :  here:[%call_pos]
  -> (confirm:bool -> Buffer.t -> unit Deferred.t)
  -> t

(** Set revert-buffer-function in [buffer]. *)
val set : Buffer.t -> t -> unit

(** Get the revert-buffer-function set in [buffer]. *)
val get : Buffer.t -> t

(** [revert_background_safe_exn] can be safely called from the background, if this buffer
    was created with a [lambda_background_safe] revert function. If not, it will raise an
    exception if called from the background. *)
val revert_background_safe_exn : Buffer.t -> unit Deferred.t
