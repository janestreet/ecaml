open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let here = [%here]
    let name = "overlay"
    let is_in_subtype = Funcall.Wrap.("overlayp" <: value @-> return bool)
  end)

let make_overlay =
  Funcall.Wrap.(
    "make-overlay" <: Position.t @-> Position.t @-> nil_or Buffer.t @-> return t)
;;

let create ?buffer () ~start ~end_ = make_overlay start end_ buffer
let start = Funcall.Wrap.("overlay-start" <: t @-> return Position.t)
let end_ = Funcall.Wrap.("overlay-end" <: t @-> return Position.t)
let buffer = Funcall.Wrap.("overlay-buffer" <: t @-> return Buffer.t)
let delete = Funcall.Wrap.("delete-overlay" <: t @-> return nil)

let move_overlay =
  Funcall.Wrap.(
    "move-overlay" <: t @-> Position.t @-> Position.t @-> nil_or Buffer.t @-> return t)
;;

let move ?buffer t ~start ~end_ = move_overlay t start end_ buffer
let overlay_get = Funcall.Wrap.("overlay-get" <: t @-> Symbol.t @-> return value)

let get_property t property_name =
  overlay_get t (property_name |> Text.Property_name.name)
  |> Text.Property_name.of_value_exn property_name
;;

let overlay_put = Funcall.Wrap.("overlay-put" <: t @-> Symbol.t @-> value @-> return nil)

let put_property t property_name property_value =
  overlay_put
    t
    (property_name |> Text.Property_name.name)
    (property_value |> Text.Property_name.to_value property_name)
;;

let remove_overlays =
  Funcall.Wrap.(
    "remove-overlays"
    <: nil_or Position.t
       @-> nil_or Position.t
       @-> nil_or Symbol.t
       @-> value
       @-> return nil)
;;

let remove_overlays ?start ?end_ ?with_property () =
  let property_name, property_value =
    match with_property with
    | None -> None, Value.nil
    | Some (name, value) ->
      Some (Text.Property_name.name name), Text.Property_name.to_value name value
  in
  remove_overlays start end_ property_name property_value
;;

let overlays_at =
  Funcall.Wrap.("overlays-at" <: Position.t @-> nil_or bool @-> return (list t))
;;

let at position = overlays_at position None

let overlays_in =
  Funcall.Wrap.("overlays-in" <: Position.t @-> Position.t @-> return (list t))
;;

let in_ ~start ~end_ = overlays_in start end_
let equal a b = Value.eq (a |> to_value) (b |> to_value)
