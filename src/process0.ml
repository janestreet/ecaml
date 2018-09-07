open! Core_kernel
open! Import0

include Value.Make_subtype (struct
    let name = "process"
    let here = [%here]
    let is_in_subtype = Value.is_process
  end)
