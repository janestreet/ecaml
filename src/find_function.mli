(** For jumping from an Elisp function name to the code that defines it, even when
    that code is Ecaml. *)

open! Core_kernel
open! Import

val initialize : unit -> unit

(** [(describe-function 'find-function)] *)
val find_function : Symbol.t -> unit
