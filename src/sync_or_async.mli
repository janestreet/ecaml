open! Core_kernel
open! Import0
open! Async_kernel

type ('a, 'b) t =
  | Sync : ('a, 'a) t
  | Async : ('a, 'a Deferred.t) t

val protect
  :  ?allow_in_background:bool (** default: false *)
  -> Source_code_position.t
  -> (_, 'a) t
  -> f:(unit -> 'a)
  -> finally:(unit -> unit)
  -> 'a
