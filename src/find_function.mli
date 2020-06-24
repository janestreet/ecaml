(** For jumping from an Elisp function name to the code that defines it, even when
    that code is Ecaml. *)

open! Core_kernel
open! Async_kernel
open! Import

(** [(describe-function 'find-function)] *)
val find_function : Symbol.t -> unit Deferred.t
