open! Core_kernel
open! Import0

module Q = struct
  include Q

  let name = "name" |> Symbol.intern
  let never = "never" |> Symbol.intern
end

include Frame0

let other_frame = Funcall.Wrap.("other-frame" <: int @-> return nil)

let create =
  let make_frame =
    Funcall.Wrap.("make-frame" <: list (tuple Symbol.t value) @-> return t)
  in
  fun ?name () ->
    let parameters =
      match name with
      | None -> []
      | Some name -> [ Q.name, name |> Value.of_utf8_bytes ]
    in
    make_frame parameters
;;

let num_cols = Funcall.Wrap.("frame-width" <: t @-> return int)
let num_rows = Funcall.Wrap.("frame-height" <: t @-> return int)
let pixel_height = Funcall.Wrap.("frame-pixel-height" <: t @-> return int)
let pixel_width = Funcall.Wrap.("frame-pixel-width" <: t @-> return int)
let frame_parameters = Funcall.Wrap.("frame-parameters" <: t @-> return (list value))

let parameters t =
  frame_parameters t
  |> List.map ~f:(fun pair ->
    if not (Value.is_cons pair)
    then
      raise_s [%message "[Frame.parameters] got strange value" ~_:(pair : Value.t)];
    Value.car_exn pair |> Symbol.of_value_exn, Value.cdr_exn pair)
;;

let is_visible = Funcall.Wrap.("frame-visible-p" <: t @-> return bool)
let all_visible = Funcall.Wrap.("visible-frame-list" <: nullary @-> return (list t))
let all_live = Funcall.Wrap.("frame-list" <: nullary @-> return (list t))

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
  Funcall.Wrap.(
    "window-list"
    <: nil_or t
       @-> nil_or Include_minibuffer.t
       @-> nil_or Window0.t
       @-> return (list Window0.t))
;;

let window_list ?include_minibuffer () = window_list None include_minibuffer None
let set_selected = Funcall.Wrap.("select-frame" <: t @-> return nil)

let set_selected_temporarily sync_or_async t ~f =
  Save_wrappers.with_selected_frame sync_or_async (to_value t) f
;;

let is_live = Funcall.Wrap.("frame-live-p" <: t @-> return bool)
let terminal = Funcall.Wrap.("frame-terminal" <: t @-> return Terminal.t)
let inherited_parameters = Var.Wrap.("frame-inherited-parameters" <: list Symbol.t)

let window_tree =
  let f =
    Funcall.Wrap.("window-tree" <: nil_or t @-> return (tuple Window0.Tree.t ignored))
  in
  fun t -> fst (f (Some t))
;;

let modify_all_frames_parameters =
  Funcall.Wrap.(
    "modify-all-frames-parameters" <: list (tuple Symbol.t value) @-> return nil)
;;
