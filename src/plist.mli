(** A property list (“plist” for short) is a list of paired elements.  Each of the pairs
    associates a property name (usually a symbol) with a property or value.

    - [(Info-goto-node "(elisp)Property Lists")] *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

(** [(describe-function 'plist-get)]
    [(Info-goto-node "(elisp)Plist Access")] *)
val get : t -> Symbol.t -> Value.t option

(** [(describe-function 'plist-put)]
    [(Info-goto-node "(elisp)Plist Access")] *)
val set : t -> Symbol.t -> Value.t -> unit

(** [(describe-function 'symbol-plist)]
    [(Info-goto-node "(elisp)Symbol Plists")] *)
val of_symbol : Symbol.t -> t
