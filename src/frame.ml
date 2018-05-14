open! Core_kernel
open! Import0

module Q = struct
  include Q
  let frame_height                     = "frame-height"                     |> Symbol.intern
  let frame_list                       = "frame-list"                       |> Symbol.intern
  let frame_parameters                 = "frame-parameters"                 |> Symbol.intern
  let frame_pixel_height               = "frame-pixel-height"               |> Symbol.intern
  let frame_pixel_width                = "frame-pixel-width"                |> Symbol.intern
  let frame_width                      = "frame-width"                      |> Symbol.intern
  let select_frame                     = "select-frame"                     |> Symbol.intern
  let selected_frame                   = "selected-frame"                   |> Symbol.intern
  let visible_frame_list               = "visible-frame-list"               |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "frame"
    let here = [%here]
    let is_in_subtype = Value.is_frame
  end)

let num_cols     t = Symbol.funcall1 Q.frame_width        (t |> to_value) |> Value.to_int_exn
let num_rows     t = Symbol.funcall1 Q.frame_height       (t |> to_value) |> Value.to_int_exn
let pixel_height t = Symbol.funcall1 Q.frame_pixel_height (t |> to_value) |> Value.to_int_exn
let pixel_width  t = Symbol.funcall1 Q.frame_pixel_width  (t |> to_value) |> Value.to_int_exn

let parameters t =
  Symbol.funcall1 Q.frame_parameters (t |> to_value)
  |> Value.to_list_exn ~f:(fun pair ->
    if not (Value.is_cons pair)
    then raise_s [%message "[Frame.parameters] got strange value" ~_:(pair : Value.t)];
    ( Value.car_exn pair |> Symbol.of_value_exn
    , Value.cdr_exn pair))
;;

let all_visible () =
  Symbol.funcall0 Q.visible_frame_list |> Value.to_list_exn ~f:of_value_exn
;;

let all_live () = Symbol.funcall0 Q.frame_list |> Value.to_list_exn ~f:of_value_exn

let selected () = Symbol.funcall0 Q.selected_frame |> of_value_exn

let set_selected t = Symbol.funcall1_i Q.select_frame (t |> to_value)
