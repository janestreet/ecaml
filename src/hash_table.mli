(** [(Info-goto-node "(elisp)Hash Tables")] *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

include Value.Subtype with type t := t

(** [(describe-function 'make-hash-table)]
    [(Info-goto-node "(elisp) Creating Hash")] *)
val create : unit -> t
