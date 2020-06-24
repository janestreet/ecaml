open! Core_kernel
open! Import

module Z = struct
  module Input_event = Input_event0
end

open Z

include Value.Make_subtype (struct
    let name = "key-sequence"
    let here = [%here]
    let is_in_subtype t = Value.is_string t || Value.is_vector t
  end)

let description = Funcall.Wrap.("key-description" <: t @-> return string)
let sexp_of_t t = [%sexp (description t : string)]
let create_exn = Funcall.Wrap.("read-kbd-macro" <: string @-> return t)
let length = Funcall.Wrap.("length" <: t @-> return int)
let get = Funcall.Wrap.("elt" <: t @-> int @-> return Input_event.t)
let to_list = Funcall.Wrap.("listify-key-sequence" <: t @-> return (list Input_event.t))
