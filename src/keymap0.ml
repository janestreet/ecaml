open! Core
open! Import

include Value.Make_subtype (struct
    let name = "keymap"
    let here = [%here]
    let is_in_subtype = Value.is_keymap
  end)
