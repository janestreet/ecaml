open! Core_kernel
open! Import
include Ocaml_or_elisp_value_intf

let make (type ocaml) (type_ : ocaml Value.Type.t) =
  ( module struct
    type nonrec ocaml = ocaml

    let sexp_of_ocaml = Value.Type.to_sexp type_
    let type_ = type_

    type abstract = Value.t [@@deriving sexp_of]

    type t =
      | Elisp of abstract
      | This of ocaml
    [@@deriving sexp_of]

    let create_elisp ocaml = Elisp (type_.to_value ocaml)

    let to_value = function
      | Elisp value -> value
      | This ocaml -> type_.to_value ocaml
    ;;
  end
  : S
    with type ocaml = ocaml )
;;
