open! Core
open! Import

include Value.Make_subtype (struct
    let name = "coding-system"
    let here = [%here]
    let is_in_subtype = Funcall.Wrap.("coding-system-p" <: value @-> return bool)
  end)

let binary = of_value_exn (Value.intern "binary")
