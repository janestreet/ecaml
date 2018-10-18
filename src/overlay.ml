open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let here = [%here]
    let name = "overlay"
    let is_in_subtype = Funcall.("overlayp" |> Symbol.intern <: value @-> return bool)
  end)

module Q = struct
  include Q

  let delete_overlay = "delete-overlay" |> Symbol.intern
  let make_overlay = "make-overlay" |> Symbol.intern
  let move_overlay = "move-overlay" |> Symbol.intern
  let overlay_buffer = "overlay-buffer" |> Symbol.intern
  let overlay_end = "overlay-end" |> Symbol.intern
  let overlay_get = "overlay-get" |> Symbol.intern
  let overlay_put = "overlay-put" |> Symbol.intern
  let overlay_start = "overlay-start" |> Symbol.intern
  let overlays_at = "overlays-at" |> Symbol.intern
  let overlays_in = "overlays-in" |> Symbol.intern
  let remove_overlays = "remove-overlays" |> Symbol.intern
end

module F = struct
  include Funcall

  let delete_overlay = Q.delete_overlay <: type_ @-> return nil

  let make_overlay =
    Q.make_overlay
    <: Position.type_ @-> Position.type_ @-> option Buffer.type_ @-> return type_
  ;;

  let move_overlay =
    Q.move_overlay
    <: type_
       @-> Position.type_
       @-> Position.type_
       @-> option Buffer.type_
       @-> return type_
  ;;

  let overlay_buffer = Q.overlay_buffer <: type_ @-> return Buffer.type_
  let overlay_end = Q.overlay_end <: type_ @-> return Position.type_
  let overlay_start = Q.overlay_start <: type_ @-> return Position.type_

  let overlays_at =
    Q.overlays_at <: Position.type_ @-> option bool @-> return (list type_)
  ;;

  let overlays_in =
    Q.overlays_in <: Position.type_ @-> Position.type_ @-> return (list type_)
  ;;
end

let create ?buffer () ~start ~end_ = F.make_overlay start end_ buffer
let start = F.overlay_start
let end_ = F.overlay_end
let buffer = F.overlay_buffer
let delete = F.delete_overlay
let move ?buffer t ~start ~end_ = F.move_overlay t start end_ buffer

let get_property t property_name =
  Symbol.funcall2
    Q.overlay_get
    (t |> to_value)
    (property_name |> Text.Property_name.name_as_value)
  |> Text.Property_name.of_value_exn property_name
;;

let put_property t property_name property_value =
  Symbol.funcall3_i
    Q.overlay_put
    (t |> to_value)
    (property_name |> Text.Property_name.name_as_value)
    (property_value |> Text.Property_name.to_value property_name)
;;

let remove_overlays ?start ?end_ ?with_property () =
  let option x f =
    match x with
    | None -> Value.nil
    | Some x -> f x
  in
  let property_name, property_value =
    match with_property with
    | None -> Value.nil, Value.nil
    | Some (name, value) ->
      Text.Property_name.name_as_value name, Text.Property_name.to_value name value
  in
  Symbol.funcall4_i
    Q.remove_overlays
    (option start Position.to_value)
    (option end_ Position.to_value)
    property_name
    property_value
;;

let at position = F.overlays_at position None
let in_ ~start ~end_ = F.overlays_in start end_
let equal a b = Value.eq (a |> to_value) (b |> to_value)
