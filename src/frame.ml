open! Core_kernel
open! Import0
include Frame0

module F = struct
  open Funcall

  let frame_live_p = Q.frame_live_p <: type_ @-> return bool
  let frame_terminal = Q.frame_terminal <: type_ @-> return Terminal.type_
  let other_frame = Q.other_frame <: int @-> return nil
end

let create () = Symbol.funcall0 Q.make_frame |> of_value_exn
let num_cols t = Symbol.funcall1 Q.frame_width (t |> to_value) |> Value.to_int_exn
let num_rows t = Symbol.funcall1 Q.frame_height (t |> to_value) |> Value.to_int_exn

let pixel_height t =
  Symbol.funcall1 Q.frame_pixel_height (t |> to_value) |> Value.to_int_exn
;;

let pixel_width t =
  Symbol.funcall1 Q.frame_pixel_width (t |> to_value) |> Value.to_int_exn
;;

let parameters t =
  Symbol.funcall1 Q.frame_parameters (t |> to_value)
  |> Value.to_list_exn ~f:(fun pair ->
    if not (Value.is_cons pair)
    then
      raise_s [%message "[Frame.parameters] got strange value" ~_:(pair : Value.t)];
    Value.car_exn pair |> Symbol.of_value_exn, Value.cdr_exn pair)
;;

let all_visible () =
  Symbol.funcall0 Q.visible_frame_list |> Value.to_list_exn ~f:of_value_exn
;;

let all_live () = Symbol.funcall0 Q.frame_list |> Value.to_list_exn ~f:of_value_exn
let set_selected t = Symbol.funcall1_i Q.select_frame (t |> to_value)
let set_selected_temporarily t ~f = Save_wrappers.with_selected_frame (to_value t) f
let is_live = F.frame_live_p
let other_frame = F.other_frame
let terminal = F.frame_terminal
