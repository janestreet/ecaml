open! Core
open! Import0

let obarrayp = Funcall.Wrap.("obarrayp" <: value @-> return bool)

include Value.Make_subtype (struct
    let name = "obarray"
    let here = [%here]
    let is_in_subtype = obarrayp
  end)

let standard = Current_buffer0.value_exn Var.Wrap.("obarray" <: t)
let mapatoms = Funcall.Wrap.("mapatoms" <: Function.t @-> t @-> return nil)

let iter t ~f =
  let f =
    Function.of_ocaml_func1 [%here] (fun symbol ->
      f (Symbol.of_value_exn symbol);
      Value.nil)
  in
  mapatoms f t
;;
