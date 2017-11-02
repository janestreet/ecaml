(** [(Info-goto-node "(elisp)Hash Tables")] *)

open! Core_kernel
open! Import

include Value.Subtype

(** [(describe-function 'make-hash-table)]
    [(Info-goto-node "(elisp) Creating Hash")] *)
val create : unit -> t
