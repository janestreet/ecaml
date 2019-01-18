open! Core_kernel
open! Import0

module Q = struct
  include Q

  let frame_height = "frame-height" |> Symbol.intern
  and frame_list = "frame-list" |> Symbol.intern
  and frame_live_p = "frame-live-p" |> Symbol.intern
  and frame_parameters = "frame-parameters" |> Symbol.intern
  and frame_pixel_height = "frame-pixel-height" |> Symbol.intern
  and frame_pixel_width = "frame-pixel-width" |> Symbol.intern
  and frame_terminal = "frame-terminal" |> Symbol.intern
  and frame_width = "frame-width" |> Symbol.intern
  and make_frame = "make-frame" |> Symbol.intern
  and other_frame = "other-frame" |> Symbol.intern
  and select_frame = "select-frame" |> Symbol.intern
  and selected_frame = "selected-frame" |> Symbol.intern
  and visible_frame_list = "visible-frame-list" |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "frame"
    let here = [%here]
    let is_in_subtype = Value.is_frame
  end)

let selected () = Symbol.funcall0 Q.selected_frame |> of_value_exn
