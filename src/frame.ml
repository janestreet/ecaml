open! Core_kernel
open! Import0
include Frame0

let other_frame = Funcall.("other-frame" <: int @-> return nil)
let create = Funcall.("make-frame" <: nullary @-> return t)
let num_cols = Funcall.("frame-width" <: t @-> return int)
let num_rows = Funcall.("frame-height" <: t @-> return int)
let pixel_height = Funcall.("frame-pixel-height" <: t @-> return int)
let pixel_width = Funcall.("frame-pixel-width" <: t @-> return int)
let frame_parameters = Funcall.("frame-parameters" <: t @-> return (list value))

let parameters t =
  frame_parameters t
  |> List.map ~f:(fun pair ->
    if not (Value.is_cons pair)
    then
      raise_s [%message "[Frame.parameters] got strange value" ~_:(pair : Value.t)];
    Value.car_exn pair |> Symbol.of_value_exn, Value.cdr_exn pair)
;;

let is_visible = Funcall.("frame-visible-p" <: t @-> return bool)
let all_visible = Funcall.("visible-frame-list" <: nullary @-> return (list t))
let all_live = Funcall.("frame-list" <: nullary @-> return (list t))
let set_selected = Funcall.("select-frame" <: t @-> return nil)

let set_selected_temporarily sync_or_async t ~f =
  Save_wrappers.with_selected_frame sync_or_async (to_value t) f
;;

let is_live = Funcall.("frame-live-p" <: t @-> return bool)
let terminal = Funcall.("frame-terminal" <: t @-> return Terminal.t)
let inherited_parameters = Var.Wrap.("frame-inherited-parameters" <: list Symbol.t)

let window_tree =
  let f =
    Funcall.("window-tree" <: nil_or t @-> return (tuple Window0.Tree.t ignored))
  in
  fun t -> fst (f (Some t))
;;
