open! Core_kernel
open! Import0

module Q = struct
  include Q

  let never = "never" |> Symbol.intern
end

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

module Include_minibuffer = struct
  module T = struct
    type t =
      | Yes
      | No
      | Only_if_active
    [@@deriving enumerate, sexp_of]
  end

  include T

  let never = Q.never |> Symbol.to_value

  let type_ =
    Value.Type.enum
      [%sexp "include-minibuffer"]
      (module T)
      (function
        | Yes -> Value.t
        | No -> never
        | Only_if_active -> Value.nil)
  ;;

  let t = type_
end

let window_list =
  Funcall.(
    "window-list"
    <: nil_or t
       @-> nil_or Include_minibuffer.t
       @-> nil_or Window0.t
       @-> return (list Window0.t))
;;

let window_list ?include_minibuffer () = window_list None include_minibuffer None
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
