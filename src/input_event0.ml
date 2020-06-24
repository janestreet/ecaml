open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "input-event"
    let here = [%here]
    let is_in_subtype = Value.is_event
  end)

let description = Funcall.Wrap.("single-key-description" <: t @-> return string)
let sexp_of_t t = [%sexp (description t : string)]
