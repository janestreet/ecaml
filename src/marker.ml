open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "marker"
    let here = [%here]
    let is_in_subtype = Value.is_marker
  end)

module Insertion_type = struct
  type t =
    | After_inserted_text
    | Before_inserted_text
  [@@deriving sexp_of]

  let of_value value =
    if value |> Value.to_bool then After_inserted_text else Before_inserted_text
  ;;

  let to_value = function
    | After_inserted_text -> Value.t
    | Before_inserted_text -> Value.nil
  ;;

  let type_ =
    Value.Type.create [%message "Marker.Insertion_type"] [%sexp_of: t] of_value to_value
  ;;

  let t = type_
end

let buffer = Funcall.Wrap.("marker-buffer" <: t @-> return (nil_or Buffer.t))

let insertion_type =
  Funcall.Wrap.("marker-insertion-type" <: t @-> return Insertion_type.t)
;;

let set_insertion_type =
  Funcall.Wrap.("set-marker-insertion-type" <: t @-> Insertion_type.t @-> return nil)
;;

let position = Funcall.Wrap.("marker-position" <: t @-> return (nil_or Position.t))
let create = Funcall.Wrap.("make-marker" <: nullary @-> return t)
let copy = Funcall.Wrap.("copy-marker" <: t @-> return t)

let set_marker =
  Funcall.Wrap.("set-marker" <: t @-> Position.t @-> Buffer.t @-> return nil)
;;

let set t buffer position = set_marker t position buffer
