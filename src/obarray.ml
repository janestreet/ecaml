open! Core
open! Import0

include Value.Make_subtype (struct
    let name = "obarray"
    let here = [%here]
    let is_in_subtype = Value.is_vector
  end)

let standard = Current_buffer0.value_exn Var.Wrap.("obarray" <: t)
let mapatoms = Funcall.Wrap.("mapatoms" <: Function.t @-> t @-> return nil)

let iter t ~f =
  let f =
    Function.create [%here] ~args:[ Q.symbol ] (fun args ->
      match args with
      | [| symbol |] ->
        f (Symbol.of_value_exn symbol);
        Value.nil
      | _ -> raise_s [%message "Expected 1 arg." (args : Value.t array)])
  in
  mapatoms f t
;;
