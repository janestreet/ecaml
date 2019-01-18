(** [Ocaml_or_elisp_value] is for calling Elisp functions, supplying either an OCaml value
    or an Elisp value.  This is useful to precompute an Elisp value from an OCaml value,
    and then to pass the Elisp value rather than repeatedly converting the same OCaml
    value to an Elisp value.  As an example, see [Completing.Collection]. *)

open! Core_kernel
open! Import

module type S = sig
  type ocaml

  val type_ : ocaml Value.Type.t

  type abstract

  type t =
    | Elisp of abstract
    | This of ocaml
  [@@deriving sexp_of]

  val create_elisp : ocaml -> t
  val to_value : t -> Value.t
end

module type Ocaml_or_elisp_value = sig
  module type S = S

  val make : 'a Value.Type.t -> (module S with type ocaml = 'a)
end
