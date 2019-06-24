open! Core_kernel
open! Import
include Ocaml_or_elisp_value_intf

let make (type ocaml) (type_ : ocaml Value.Type.t) =
  (module struct
    type nonrec ocaml = ocaml

    let sexp_of_ocaml = Value.Type.to_sexp type_
    let type_ = type_

    type abstract = Value.t [@@deriving sexp_of]

    type t =
      | Elisp of abstract
      | This of ocaml
    [@@deriving sexp_of]

    let create_elisp ocaml = Elisp (Value.Type.to_value type_ ocaml)

    let to_value = function
      | Elisp value -> value
      | This ocaml -> Value.Type.to_value type_ ocaml
    ;;
  end : S
    with type ocaml = ocaml)
;;
