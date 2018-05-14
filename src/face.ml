open! Core_kernel
open! Import0

module Q = struct
  include Q
  let bold                             = "bold"                             |> Symbol.intern
  let condensed                        = "condensed"                        |> Symbol.intern
  let expanded                         = "expanded"                         |> Symbol.intern
  let extra_bold                       = "extra-bold"                       |> Symbol.intern
  let extra_condensed                  = "extra-condensed"                  |> Symbol.intern
  let extra_expanded                   = "extra-expanded"                   |> Symbol.intern
  let extra_light                      = "extra-light"                      |> Symbol.intern
  let face_all_attributes              = "face-all-attributes"              |> Symbol.intern
  let face_attribute                   = "face-attribute"                   |> Symbol.intern
  let face_attribute_relative_p        = "face-attribute-relative-p"        |> Symbol.intern
  let face_list                        = "face-list"                        |> Symbol.intern
  let face_spec_set                    = "face-spec-set"                    |> Symbol.intern
  let font_family_list                 = "font-family-list"                 |> Symbol.intern
  let italic                           = "italic"                           |> Symbol.intern
  let light                            = "light"                            |> Symbol.intern
  let merge_face_attribute             = "merge-face-attribute"             |> Symbol.intern
  let normal                           = "normal"                           |> Symbol.intern
  let oblique                          = "oblique"                          |> Symbol.intern
  let reverse_italic                   = "reverse-italic"                   |> Symbol.intern
  let reverse_oblique                  = "reverse-oblique"                  |> Symbol.intern
  let semi_bold                        = "semi-bold"                        |> Symbol.intern
  let semi_condensed                   = "semi-condensed"                   |> Symbol.intern
  let semi_expanded                    = "semi-expanded"                    |> Symbol.intern
  let semi_light                       = "semi-light"                       |> Symbol.intern
  let set_face_attribute               = "set-face-attribute"               |> Symbol.intern
  let ultra_bold                       = "ultra-bold"                       |> Symbol.intern
  let ultra_condensed                  = "ultra-condensed"                  |> Symbol.intern
  let ultra_expanded                   = "ultra-expanded"                   |> Symbol.intern
  let ultra_light                      = "ultra-light"                      |> Symbol.intern
  let unspecified                      = "unspecified"                      |> Symbol.intern
end

module Value = struct
  include Value

  let unspecified = Q.unspecified |> Symbol.to_value
end

include Value.Make_subtype (struct
    let name = "face"
    let here = [%here]
    let is_in_subtype = Value.is_symbol
  end)

let of_name s = s |> Value.intern |> of_value_exn

let default = "default" |> of_name

let to_name t =
  let v = t |> to_value in
  match Symbol.of_value_exn v with
  | s -> s |> Symbol.name
  | exception _ ->
    raise_s [%message "[Face.to_name] got unexpected value" ~_:(v : Value.t)]
;;

let compare t1 t2 = String.compare (t1 |> to_name) (t2 |> to_name)

let equal = eq

module type Attribute_value = sig
  type t [@@deriving sexp_of]

  val symbol : Symbol.t

  val of_value_exn : Value.t -> t
  val to_value     : t -> Value.t

  val unspecified : t
end

module Unimplemented = struct
  type t = Value.t [@@deriving sexp_of]
  let of_value_exn = Fn.id
  let to_value = Fn.id
  let unspecified = Value.unspecified
end

module Color_or_unspecified = struct
  type t =
    | Color of Color.t
    | Unspecified
  [@@deriving sexp_of]

  let unspecified = Unspecified

  let to_value = function
    | Color c     -> c |> Color.to_value
    | Unspecified -> Value.unspecified
  ;;

  let of_value_exn value =
    if Value.eq value Value.unspecified
    (* Sometimes Emacs returns an unspecified color as the string "unspecified-fg" or
       "unspecified-bg" rather than the symbol [unspecified]. *)
    || (Value.is_string value
        && String.is_prefix ~prefix:"unspecified" (Value.to_utf8_bytes_exn value))
    then Unspecified
    else Color (value |> Color.of_value_exn)
  ;;
end

module String_name = struct
  type t =
    | Name of string
    | Unspecified
  [@@deriving sexp_of]

  let unspecified = Unspecified

  let of_value_exn value =
    if Value.eq value Value.unspecified
    then Unspecified
    else Name (value |> Value.to_utf8_bytes_exn)
  ;;

  let to_value = function
    | Name s -> s |> Value.of_utf8_bytes
    | Unspecified -> Value.unspecified
  ;;
end

module Background = struct
  include Color_or_unspecified
  let symbol = Q.K.background
end

module Box = struct
  let symbol = Q.K.box
  include Unimplemented
end

module Font = struct
  let symbol = Q.K.font
  include Unimplemented
end

module Font_family = struct
  let symbol = Q.K.family
  include String_name
end

module Font_foundry = struct
  let symbol = Q.K.foundry
  include String_name
end

module Foreground = struct
  include Color_or_unspecified
  let symbol = Q.K.foreground
end

module Height = struct
  type t =
    | Scale_underlying_face of float
    | Tenths_of_point       of int
    | Unspecified
  [@@deriving sexp_of]

  let unspecified = Unspecified

  let symbol = Q.K.height

  let of_value_exn value =
    if Value.eq value Value.unspecified
    then Unspecified
    else
      match Value.to_int_exn value with
      | i -> Tenths_of_point i
      | exception _ ->
        match Value.to_float_exn value with
        | f -> Scale_underlying_face f
        | exception _ ->
          raise_s [%message
            "[Face.Height.of_value_exn] got unexpected value" (value : Value.t)]
  ;;

  let to_value = function
    | Scale_underlying_face f -> f |> Value.of_float
    | Tenths_of_point       i -> i |> Value.of_int_exn
    | Unspecified             -> Value.unspecified
  ;;
end

module Inherit = struct
  let symbol = Q.K.inherit_

  type nonrec t =
    | Face of t
    | Unspecified
  [@@deriving sexp_of]

  let unspecified = Unspecified

  let to_value = function
    | Face face   -> to_value face
    | Unspecified -> Value.unspecified
  ;;

  let of_value_exn value =
    (* It's possible to (defface nil) and then even to (describe-face nil), but applying
       the nil face and inheriting from the nil face have no effect. *)
    if Value.eq value Value.unspecified || Value.eq value Value.nil
    then Unspecified
    else Face (of_value_exn value)
  ;;
end

module Inverse_video = struct
  let symbol = Q.K.inverse_video

  type t =
    | No
    | Yes
    | Unspecified
  [@@deriving sexp_of]

  let unspecified = Unspecified

  let to_value = function
    | No          -> Value.nil
    | Yes         -> Value.t
    | Unspecified -> Value.unspecified
  ;;

  let of_value_exn value =
    if Value.is_nil value
    then No
    else if Value.eq value Value.t
    then Yes
    else if Value.eq value Value.unspecified
    then Unspecified
    else raise_s [%message
           "[Face.Inverse_video.of_value_exn] got unexpected value" (value : Value.t)]
  ;;
end

module Line = struct
  type t =
    | Absent
    | Color of Color.t
    | Foreground
    | Unspecified
  [@@deriving sexp_of]

  let unspecified = Unspecified

  let of_value_exn value =
    if Value.eq value Value.unspecified
    then Unspecified
    else if Value.is_nil value
    then Absent
    else if Value.eq value Value.t
    then Foreground
    else
      match Color.of_value_exn value with
      | c -> Color c
      | exception _ ->
        raise_s [%message
          "[Face.Line.of_value_exn] got unexpected value" (value : Value.t)]
  ;;

  let to_value = function
    | Absent      -> Value.nil
    | Color c     -> c |> Color.to_value
    | Foreground  -> Value.t
    | Unspecified -> Value.unspecified
  ;;
end

module Overline = struct
  include Line
  let symbol = Q.K.overline
end

module Slant = struct
  module T = struct
    let module_name = "Face.Slant"

    let symbol = Q.K.slant

    type t =
      | Italic
      | Oblique
      | Normal
      | Reverse_italic
      | Reverse_oblique
      | Unspecified
    [@@deriving enumerate, sexp_of]

    let unspecified = Unspecified

    let to_symbol = function
      | Italic          -> Q.italic
      | Oblique         -> Q.oblique
      | Normal          -> Q.normal
      | Reverse_italic  -> Q.reverse_italic
      | Reverse_oblique -> Q.reverse_oblique
      | Unspecified     -> Q.unspecified
    ;;
  end

  include T
  include Symbol.Make_subtype (T)
end

module Stipple = struct
  let symbol = Q.K.stipple
  include Unimplemented
end

module Strike_through = struct
  let symbol = Q.K.strike_through
  include Line
end

module Underline = struct
  let symbol = Q.K.underline
  include Line
end

module Weight = struct
  module T = struct
    let module_name = "Face.Weight"
    let symbol = Q.K.weight

    type t =
      | Ultra_bold
      | Extra_bold
      | Bold
      | Semi_bold
      | Normal
      | Semi_light
      | Light
      | Extra_light
      | Ultra_light
      | Unspecified
    [@@deriving enumerate, sexp_of]

    let unspecified = Unspecified

    let to_symbol = function
      | Ultra_bold  -> Q.ultra_bold
      | Extra_bold  -> Q.extra_bold
      | Bold        -> Q.bold
      | Semi_bold   -> Q.semi_bold
      | Normal      -> Q.normal
      | Semi_light  -> Q.semi_light
      | Light       -> Q.light
      | Extra_light -> Q.extra_light
      | Ultra_light -> Q.ultra_light
      | Unspecified -> Q.unspecified
    ;;
  end

  include T
  include Symbol.Make_subtype (T)
end

module Width = struct
  module T = struct
    let module_name = "Face.Width"
    let symbol = Q.K.width

    type t =
      | Ultra_condensed
      | Extra_condensed
      | Condensed
      | Semi_condensed
      | Normal
      | Semi_expanded
      | Expanded
      | Extra_expanded
      | Ultra_expanded
      | Unspecified
    [@@deriving enumerate, sexp_of]

    let unspecified = Unspecified

    let to_symbol = function
      | Ultra_condensed -> Q.ultra_condensed
      | Extra_condensed -> Q.extra_condensed
      | Condensed       -> Q.condensed
      | Semi_condensed  -> Q.semi_condensed
      | Normal          -> Q.normal
      | Semi_expanded   -> Q.semi_expanded
      | Expanded        -> Q.expanded
      | Extra_expanded  -> Q.extra_expanded
      | Ultra_expanded  -> Q.ultra_expanded
      | Unspecified     -> Q.unspecified
    ;;
  end

  include T
  include Symbol.Make_subtype (T)
end

module Attribute = struct
  type _ t =
    | Background     : Background.t     t
    | Box            : Box.t            t
    | Font           : Font.t           t
    | Font_family    : Font_family.t    t
    | Font_foundry   : Font_foundry.t   t
    | Foreground     : Foreground.t     t
    | Height         : Height.t         t
    | Inherit        : Inherit.t        t
    | Inverse_video  : Inverse_video.t  t
    | Overline       : Overline.t       t
    | Slant          : Slant.t          t
    | Stipple        : Stipple.t        t
    | Strike_through : Strike_through.t t
    | Underline      : Underline.t      t
    | Weight         : Weight.t         t
    | Width          : Width.t          t
  [@@deriving sexp_of]

  type 'a attribute = 'a t

  let value_module : type a. a t -> (module Attribute_value with type t = a) = function
    | Background     -> (module Background)
    | Box            -> (module Box)
    | Font           -> (module Font)
    | Font_family    -> (module Font_family)
    | Font_foundry   -> (module Font_foundry)
    | Foreground     -> (module Foreground)
    | Height         -> (module Height)
    | Inherit        -> (module Inherit)
    | Inverse_video  -> (module Inverse_video)
    | Overline       -> (module Overline)
    | Slant          -> (module Slant)
    | Stipple        -> (module Stipple)
    | Strike_through -> (module Strike_through)
    | Underline      -> (module Underline)
    | Weight         -> (module Weight)
    | Width          -> (module Width)
  ;;

  let unspecified_value (type a) (t : a t) : a =
    let module Value = (val value_module t) in
    Value.unspecified
  ;;

  let to_symbol (type a) (t : a t) =
    let module Value = (val value_module t) in
    Value.symbol
  ;;

  let compare_name t1 t2 =
    String.compare
      (t1 |> to_symbol |> Symbol.name)
      (t2 |> to_symbol |> Symbol.name)
  ;;

  module Packed = struct

    module T = struct
      let module_name = "Face.Attribute.Packed"

      type t = T : _ attribute -> t

      let sexp_of_t (T a) = [%sexp (a : _ t)]

      (* The type system doesn't guarantee that [all] is exhaustive.  But the tests of
         [Face.attributes] in [test_face.ml] do ensure this, because they convert
         attributes as symbols to [Face.Attribute.Packed.t] using [Packed.of_symbol_exn],
         which looks up attributes based on [all]. *)
      let all =
        [ T Background
        ; T Box
        ; T Font
        ; T Font_family
        ; T Font_foundry
        ; T Foreground
        ; T Height
        ; T Inherit
        ; T Inverse_video
        ; T Overline
        ; T Slant
        ; T Stipple
        ; T Strike_through
        ; T Underline
        ; T Weight
        ; T Width ]
      ;;

      let to_symbol (T a) = to_symbol a
    end

    include T
    include Symbol.Make_subtype (T)
  end

  let of_value_exn (type a) (t : a t) : Value.t -> a =
    let module Value = (val value_module t) in
    Value.of_value_exn
  ;;

  let to_value (type a) (t : a t) : a -> Value.t =
    let module Value = (val value_module t) in
    Value.to_value
  ;;

  let is_relative t a =
    Symbol.funcall2 Q.face_attribute_relative_p
      (t |> to_symbol |> Symbol.to_value)
      (a |> to_value t)
    |> Value.to_bool
  ;;

  let merge t a1 a2 =
    Symbol.funcall3 Q.merge_face_attribute
      (t |> to_symbol |> Symbol.to_value)
      (a1 |> to_value t)
      (a2 |> to_value t)
    |> of_value_exn t
  ;;
end

module Attribute_and_value = struct
  type t = T : 'a Attribute.t * 'a -> t

  let sexp_of_t (T (attribute, value)) =
    let module Value = (val Attribute.value_module attribute) in
    [%message "" ~_:(attribute : _ Attribute.t) ~_:(value : Value.t)]
  ;;

  let compare_attribute_name (T (a1, _)) (T (a2, _)) = Attribute.compare_name a1 a2

  let of_value_exn value =
    match Value.car_exn value with
    | attribute ->
      let Attribute.Packed.T attribute = attribute |> Attribute.Packed.of_value_exn in
      T (attribute, Attribute.of_value_exn attribute (Value.cdr_exn value))
    | exception _ ->
      raise_s [%message
        "[Face.Attribute_and_value.of_value_exn] got unexpected value" (value : Value.t)]
  ;;

  let sort_by_attribute_name ts =
    List.sort ts
      ~compare:(fun (T (a1, _)) (T (a2, _)) -> Attribute.compare_name a1 a2)
  ;;

  let to_value_list (T (attribute, value)) =
    [ Attribute.to_symbol attribute |> Symbol.to_value
    ; Attribute.to_value attribute value
    ]
end

let frame option =
  (match option with
   | Some x -> x
   | None -> Frame.selected ())
  |> Frame.to_value
;;

let all_defined () =
  Symbol.funcall0 Q.face_list
  |> Value.to_list_exn ~f:of_value_exn
  |> List.sort ~compare
;;

let font_family_list ?on () =
  Symbol.funcall1 Q.font_family_list (frame on)
  |> Value.to_list_exn ~f:Value.to_utf8_bytes_exn
;;

let attribute_value ?on t attribute =
  Symbol.funcall3 Q.face_attribute
    (t |> to_value)
    (attribute |> Attribute.to_symbol |> Symbol.to_value)
    (frame on)
  |> Attribute.of_value_exn attribute
;;

let set_attribute ?on t attribute value =
  Symbol.funcall4_i Q.set_face_attribute
    (t |> to_value)
    (frame on)
    (attribute |> Attribute.to_symbol |> Symbol.to_value)
    (Attribute.to_value attribute value)
;;

let attributes ?on t =
  Symbol.funcall2 Q.face_all_attributes (t |> to_value) (frame on)
  |> Value.to_list_exn ~f:Attribute_and_value.of_value_exn
;;

let spec_set face specs =
  Symbol.funcall2_i Q.face_spec_set
    (to_value face)
    Value.(list [ cons t (list (List.concat_map specs
                                  ~f:Attribute_and_value.to_value_list)) ])
;;
