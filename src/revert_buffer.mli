open! Core
open! Async_kernel
open! Import

type t

(** Define a function which is suitable for use as revert-buffer-function.

    [(describe-variable 'revert-buffer-function)] *)
val define
  :  ?here:Stdlib.Lexing.position
  -> Symbol.t
  -> (confirm:bool -> unit Deferred.t)
  -> t

val lambda : ?here:Stdlib.Lexing.position -> (confirm:bool -> unit Deferred.t) -> t

(** Like [define], but the revert function can be safely called in the background by
    Async, with a [Buffer.t] argument. *)
val define_background_safe
  :  ?here:Stdlib.Lexing.position
  -> Symbol.t
  -> (confirm:bool -> Buffer.t -> unit Deferred.t)
  -> t

(** Set revert-buffer-function in [buffer]. *)
val set : Buffer.t -> t -> unit

(** [revert_background_safe_exn] can be safely called from the background, if this buffer
    was created with a [define_background_safe] revert function. If not, it will raise an
    exception if called from the background. *)
val revert_background_safe_exn : Buffer.t -> unit Deferred.t
