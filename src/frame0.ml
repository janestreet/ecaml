open! Core_kernel
open! Import0

include Value.Make_subtype (struct
    let name = "frame"
    let here = [%here]
    let is_in_subtype = Value.is_frame
  end)

let selected = Funcall.Wrap.("selected-frame" <: nullary @-> return t)
