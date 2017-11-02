(**  [(Info-goto-node "(elisp)System Environment")] *)

open! Core_kernel
open! Import

(** [(describe-function 'getenv)]
    [(Info-goto-node "(elisp)System Environment")] *)
val getenv : var: string -> string option

(** [(describe-function 'setenv)]
    [(Info-goto-node "(elisp)System Environment")] *)
val setenv : var : string -> value : string option -> unit

(** [(describe-variable 'process-environment)]
    [(Info-goto-node "(elisp)System Environment")] *)
val process_environment : string list Var.t

module Var_and_value : sig
  type t =
    { var   : string
    ; value : string }
  [@@deriving sexp_of]
end

val setenv_temporarily : Var_and_value.t list -> f:(unit -> 'a) -> 'a
