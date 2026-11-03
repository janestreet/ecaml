(** A typed Deferred that can be returned to Elisp without blocking. *)

open! Core
open! Async_kernel
open! Import0

(** The return type for an Ecaml defun that returns a Deferred whose eventual result has
    type [result_type]. The result remains an OCaml value until Elisp calls
    [ecaml-deferred-wait].

    Reading the Deferred back with [Value.Type.of_value_exn] requires using the exact same
    [result_type] value. A separately created, structurally equivalent [Value.Type.t] has
    a different nominal type ID and will be rejected. *)
val type_ : 'a Value.Type.t -> 'a Deferred.Or_error.t Value.Type.t
