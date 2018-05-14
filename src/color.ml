open! Core_kernel
open! Import0

module Q = struct
  include Q
  let color_defined_p                  = "color-defined-p"                  |> Symbol.intern
  let color_gray_p                     = "color-gray-p"                     |> Symbol.intern
  let color_supported_p                = "color-supported-p"                |> Symbol.intern
  let color_values                     = "color-values"                     |> Symbol.intern
  let defined_colors                   = "defined-colors"                   |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "color"
    let here = [%here]
    let is_in_subtype = Value.is_string
  end)

let to_name t = t |> to_value |> Value.to_utf8_bytes_exn

let compare t1 t2 = String.compare (t1 |> to_name) (t2 |> to_name)

let of_name s = s |> Value.of_utf8_bytes |> of_value_exn

let black   = "black"   |> of_name
let blue    = "blue"    |> of_name
let cyan    = "cyan"    |> of_name
let green   = "green"   |> of_name
let magenta = "magenta" |> of_name
let orange  = "orange"  |> of_name
let red     = "red"     |> of_name
let white   = "white"   |> of_name
let yellow  = "yellow"  |> of_name

let frame option =
  (match option with
   | Some x -> x
   | None -> Frame.selected ())
  |> Frame.to_value
;;

let is q ?on t = Symbol.funcall2 q (t |> to_value) (frame on) |> Value.to_bool

let is_gray      = is Q.color_gray_p
let is_defined   = is Q.color_defined_p
let is_supported = is Q.color_supported_p

let defined ?on () =
  Symbol.funcall1 Q.defined_colors (frame on)
  |> Value.to_list_exn ~f:of_value_exn
  |> List.sort ~compare
;;

module RGB = struct
  type t =
    { r : int
    ; g : int
    ; b : int }
  [@@deriving sexp_of]

  let min_value = 0

  let max_value = 1 lsl 16 - 1

  let clamp i = Int.max min_value (Int.min max_value i)

  let map { r; g; b } ~f =
    { r = f r
    ; g = f g
    ; b = f b }
  ;;
end

let elt = Generated_bindings.elt_returning_int

let rgb_exn ?on t : RGB.t =
  let v = Symbol.funcall2 Q.color_values (t |> to_value) (frame on) in
  if Value.is_nil v
  then raise_s [%message.omit_nil
         "[Color.rgb_exn] got non-displayable color"
           ~color:(t : t) (on : Frame.t option)];
  let elt i = elt v i in
  { r = elt 0
  ; g = elt 1
  ; b = elt 2 }
;;

let of_rgb { RGB. r; g; b } =
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
