(** [(Info-goto-node "(elisp)Display Property")] *)
open! Core_kernel

open! Import

module Margin : sig
  type t =
    | Left
    | Right
  [@@deriving sexp_of]
end

type t = Display_in_margin of Margin.t [@@deriving sexp_of]

val to_values : t -> Value.t list
val of_values_exn : Value.t * Value.t -> t
