(** A "face" is a collection of graphical attributes for displaying text: font family,
    foreground color, background color, optional underlining, and so on.  Faces control
    how buffer text is displayed, and how some parts of the frame, such as the mode-line,
    are displayed.

    For most purposes, you refer to a face in Lisp programs using its "face name".  Each
    face name is meaningful for all frames, and by default it has the same meaning in all
    frames.  But you can arrange to give a particular face name a special meaning in one
    frame if you wish.

    [(Info-goto-node "(elisp)Faces")] *)

open! Core_kernel
open! Import0

include Value.Subtype

include Equal.S with type t := t

val default : t

val of_name : string -> t
val to_name : t -> string

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Background : sig
  type t =
    | Color of Color.t
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Box : sig
  type t [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Font : sig
  type t [@@deriving sexp_of]
end

(** Font family name or fontset name; the wild-card characters `*' and `?' are allowed.
    [(Info-goto-node "(elisp)Face Attributes")]. *)
module Font_family : sig
  type t =
    | Name of string
    | Unspecified
  [@@deriving sexp_of]
end

(** The name of the "font foundry" in which the font family attribute is located; the
    wild-card characters `*' and `?' are allowed.  [(Info-goto-node "(elisp)Face
    Attributes")] *)
module Font_foundry : sig
  type t =
    | Name of string
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Foreground : sig
  type t =
    | Color of Color.t
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Height : sig
  type t =
    | Scale_underlying_face of float
    | Tenths_of_point       of int
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Inherit : sig
  type nonrec t =
    | Face of t
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Inverse_video : sig
  type t =
    | No
    | Yes
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Overline : sig
  type t =
    | Absent
    | Color of Color.t
    | Foreground
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Slant : sig
  type t =
    | Italic
    | Oblique
    | Normal
    | Reverse_italic
    | Reverse_oblique
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Stipple : sig
  type t [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Strike_through : sig
  type t =
    | Absent
    | Color of Color.t
    | Foreground
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Underline : sig
  type t =
    | Absent
    | Color of Color.t
    | Foreground
    | Unspecified
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Weight : sig
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
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Width : sig
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
  [@@deriving sexp_of]
end

(** [(Info-goto-node "(elisp)Face Attributes")] *)
module Attribute : sig
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

  val to_symbol : _ t -> Symbol.t

  val of_value_exn : 'a t -> Value.t -> 'a

  val to_value : 'a t -> 'a -> Value.t

  val unspecified_value : 'a t -> 'a

  module Packed : sig
    type 'a attribute = 'a t
    type t = T : _ attribute -> t

    include Symbol.Subtype with type t := t

    val all : t list
  end with type 'a attribute := 'a t

  (** A relative value is one that doesn’t entirely override whatever is inherited from
      another face.  For most attributes, the only relative value is [Unspecified].
      [Height.Scale_underlying_face] values are also relative.  [(describe-function
      'face-attribute-relative-p)]. *)
  val is_relative : 'a t -> 'a -> bool

  (** [(describe-function 'merge-face-attribute)] *)
  val merge : 'a t -> 'a -> 'a -> 'a
end

module Attribute_and_value : sig
  type t = T : 'a Attribute.t * 'a -> t
  [@@deriving sexp_of]

  val of_value_exn : Value.t -> t

  val compare_attribute_name : t -> t -> int

  val sort_by_attribute_name : t list -> t list
end

(** [(describe-function 'face-list)] *)
val all_defined : unit -> t list

(** [(describe-function 'face-attribute)] *)
val font_family_list
  :  ?on : Frame.t  (** default is selected frame *)
  -> unit
  -> string list

(** [(describe-function 'face-attribute)] *)
val attribute_value
  :  ?on : Frame.t  (** default is selected frame *)
  -> t
  -> 'a Attribute.t
  -> 'a

(** [(describe-function 'set-face-attribute)] *)
val set_attribute
  :  ?on : Frame.t
  -> t
  -> 'a Attribute.t
  -> 'a
  -> unit

(** [(describe-function 'face-all-attributes)] *)
val attributes
  :  ?on : Frame.t  (** default is selected frame *)
  -> t
  -> Attribute_and_value.t list

(** [(describe-function 'face-spec-set)] *)
val spec_set : t -> Attribute_and_value.t list -> unit
