(** An obarray is a hash table mapping strings to [Symbol.t]s.  [Symbol.intern] and
    [Form.read] use an obarray.

    [(Info-goto-node "(elisp)Creating Symbols")] *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

(** [(describe-variable 'obarray)]
    [(Info-goto-node "(elisp)Creating Symbols")] *)
val standard : t

(** [(describe-function 'mapatoms)]
    [(Info-goto-node "(elisp)Creating Symbols")] *)
val iter : t -> f:(Symbol.t -> unit) -> unit
