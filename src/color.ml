open! Core_kernel
open! Import0
module Frame = Frame0

include Value.Make_subtype (struct
    let name = "color"
    let here = [%here]
    let is_in_subtype = Value.is_string
  end)

let to_name t = t |> to_value |> Value.to_utf8_bytes_exn
let compare t1 t2 = String.compare (t1 |> to_name) (t2 |> to_name)
let of_name s = s |> Value.of_utf8_bytes |> of_value_exn

let black = "black" |> of_name
and blue = "blue" |> of_name
and cyan = "cyan" |> of_name
and green = "green" |> of_name
and magenta = "magenta" |> of_name
and orange = "orange" |> of_name
and red = "red" |> of_name
and white = "white" |> of_name
and yellow = "yellow" |> of_name

let is f ?on t = f t on
let is_gray = is Funcall.("color-gray-p" <: t @-> nil_or Frame.t @-> return bool)
let is_defined = is Funcall.("color-defined-p" <: t @-> nil_or Frame.t @-> return bool)

let is_supported =
  is Funcall.("color-supported-p" <: t @-> nil_or Frame.t @-> return bool)
;;

let defined_colors = Funcall.("defined-colors" <: nil_or Frame.t @-> return (list t))
let defined ?on () = defined_colors on |> List.sort ~compare

module RGB = struct
  type t =
    { r : int
    ; g : int
    ; b : int
    }
  [@@deriving sexp_of]

  let min_value = 0
  let max_value = (1 lsl 16) - 1
  let clamp i = Int.max min_value (Int.min max_value i)
  let map { r; g; b } ~f = { r = f r; g = f g; b = f b }
end

let elt = Generated_bindings.elt_returning_int
let color_values = Funcall.("color-values" <: t @-> nil_or Frame.t @-> return value)

let rgb_exn ?on t : RGB.t =
  let v = color_values t on in
  if Value.is_nil v
  then
    raise_s
      [%message.omit_nil
        "[Color.rgb_exn] got non-displayable color" ~color:(t : t) (on : Frame.t option)];
  let elt i = elt v i in
  { r = elt 0; g = elt 1; b = elt 2 }
;;

let of_rgb { RGB.r; g; b } =
  let p c = sprintf "%04X" (RGB.clamp c) in
  of_name (concat [ "#"; p r; p g; p b ])
;;

let of_rgb8 ~r ~g ~b =
  let min_value = 0 in
  let max_value = 255 in
  let clamp i = Int.max min_value (Int.min max_value i) in
  let p c = sprintf "%02X" (clamp c) in
  of_name (concat [ "#"; p r; p g; p b ])
;;
