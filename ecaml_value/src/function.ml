open! Core
open! Import

include Value.Make_subtype (struct
    let name = "function"
    and here = [%here]
    and is_in_subtype v = Value.is_symbol v || Value.is_function v
  end)

module Fn = struct
  type t = Value.t array -> Value.t [@@deriving sexp_of]
end

external make_function : int -> int option -> Fn.t -> Value.t = "ecaml_make_function"

let make_function ~min_args ~max_args f =
  make_function min_args max_args (fun args ->
    try f args with
    | exn ->
      Value.Expert.non_local_exit_signal exn;
      Value.nil)
;;

let fset = Funcall.Wrap.("fset" <: Symbol.t @-> value @-> return nil)

let of_ocaml_funcN_M here ~min_args ~max_args f =
  let sym =
    Symbol.create_uninterned ~name:[%string "ecaml-func-%{here#Source_code_position}"]
  in
  fset sym (make_function ~min_args ~max_args f);
  sym |> Symbol.to_value |> of_value_exn
;;

let of_ocaml_funcN here ~nargs f =
  of_ocaml_funcN_M here ~min_args:nargs ~max_args:(Some nargs) f
;;

(* This should basically never be called. If it is called, that means [nargs] does not
   match the number of arguments that the function actually requires. *)
let[@cold] raise_unexpected_arguments ~expected ~actual =
  let actual = Array.length actual in
  raise_s [%message "Ecaml bug: unexpected arguments" (expected : int) (actual : int)]
;;

let of_ocaml_func0 here f =
  of_ocaml_funcN here ~nargs:0 (function
    | [||] -> f ()
    | actual -> raise_unexpected_arguments ~expected:0 ~actual)
;;

let of_ocaml_func1 here f =
  of_ocaml_funcN here ~nargs:1 (function
    | [| v |] -> f v
    | actual -> raise_unexpected_arguments ~expected:1 ~actual)
;;

let of_ocaml_func2 here f =
  of_ocaml_funcN here ~nargs:2 (function
    | [| v1; v2 |] -> f v1 v2
    | actual -> raise_unexpected_arguments ~expected:2 ~actual)
;;

let of_symbol_exn symbol =
  let value = Symbol.to_value symbol in
  assert (Value.is_function value);
  of_value_exn value
;;

let of_symbol_unsafe symbol =
  let value = Symbol.to_value symbol in
  of_value_exn value
;;
