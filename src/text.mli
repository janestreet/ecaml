(** Emacs text, which is a sequence of characters, along with text properties associated
    with each character.

    [(Info-goto-node "(elisp)Text")] *)

open! Core
open! Import
include Value.Subtype

type text := t

module Compare_as_string : sig
  include Comparable.S with type t = t
  include Sexpable.S with type t := t
end

(** [(describe-function 'aref)] [(Info-goto-node "(elisp)String Basics")] *)
val char_code : t -> int -> Char_code.t

(** [(describe-function 'aset)] [(Info-goto-node "(elisp)String Basics")] *)
val set_char_code : t -> int -> Char_code.t -> unit

val of_utf8_bytes : string -> t

val of_utf8_bytes_replacing_invalid
  :  string
  -> t * [ `First_malformed of (int * string) option ]

val to_utf8_bytes : t -> string

(** [length t] returns the number of characters in [t]. *)
val length : t -> int

(** [num_bytes t] returns the number of bytes in the representation of [t], which, if
    [is_multibyte t], can be greater than [length t]. [(describe-function 'string-bytes)] *)
val num_bytes : t -> int

(** [(describe-function 'concat)] *)
val concat : ?sep:t -> t list -> t

(** [(describe-function 'substring)]

    [start] is inclusive, [end_] is exclusive. *)
val substring : t -> start:int -> end_:int -> t

(** [(Info-goto-node "(elisp)Special Properties")] *)
module Face_spec : sig
  module One : sig
    type t =
      | Attributes of Face.Attribute_and_value.t list
      | Face of Face.t
    [@@deriving sexp_of]

    include Valueable.S with type t := t
  end

  type t = One.t list [@@deriving sexp_of]

  include Valueable.S with type t := t

  (** [normalize] sorts [t]; it is used to get consistent ordering when comparing
      [Ansi_color] to Elisp's [ansi-color] functionality. *)
  val normalize : t -> t
end

module Display_spec : sig
  (** Display specs apply a property, like [margin], to a text. In Elisp, one would write,
      e.g., [(propertize " " 'display `((margin left-margin) ,str))] where [str] is a
      text. A display spec is therefore a pair of a property and the text it's applied to. *)
  type nonrec t =
    { property : Display_property.t
    ; text : t
    }
  [@@deriving sexp_of]

  val of_value_exn : Value.t -> t
  val to_value : t -> Value.t
end

(** [(Info-goto-node "(elisp)Text Properties")] *)
module Property_name : sig
  type 'a t [@@deriving sexp_of]
  type 'a property_name := 'a t

  (** [(Info-goto-node "(elisp)Special Properties")] *)
  val face : Face_spec.t t

  val mouse_face : Face_spec.t t
  val font_lock_face : Face_spec.t t
  val display : Display_spec.t t
  val after_string : text t
  val before_string : text t
  val invisible : Value.t t

  (** value automatically displayed by Emacs on mouse-over *)
  val help_echo : string t

  (** value automatically displayed by Emacs when text at point has the property, if
      [help-at-pt-display-when-idle] is enabled *)
  val kbd_help : string t

  val name : _ t -> Symbol.t
  val name_as_value : _ t -> Value.t
  val to_value : 'a t -> 'a -> Value.t
  val of_value_exn : 'a t -> Value.t -> 'a

  (** [(<:)] creates a new property name and makes it available for automatic typeful
      conversion of elisp property lists. Idiomatic usage looks like this:
      {[
        Text.Property_name.Create.(name <: type_)
      ]}
      We keep [Create] clear of other values to avoid polluting the namespace inside that
      local open. *)
  module Create : sig
    val ( <: ) : string -> 'a Value.Type.t -> 'a t
  end

  module Packed : sig
    type t = T : _ property_name -> t [@@deriving sexp_of]

    val name : t -> Symbol.t
  end
end

(** [(Info-goto-node "(elisp)Text Properties")] *)
module Property : sig
  type t = T : 'a Property_name.t * 'a -> t [@@deriving sexp_of]

  val to_property_list : t list -> Value.t list
end

(** [(describe-function 'propertize)] [(Info-goto-node "(elisp)Changing Properties")] *)
val propertize : t -> Property.t list -> t

val colorize : t -> color:Color.t -> t

(** [property_value t ~at property_name] returns the value of [property_name] for the
    character after [at]. [at] is a zero-based index into [t]; [property_value] raises
    unless [0 <= at <= length t] (allowing [length] is different than OCaml).
    [(describe-function 'get-text-property)]
    [(Info-goto-node "(elisp)Examining Properties")] *)
val property_value : t -> at:int -> 'a Property_name.t -> 'a option

(** [(describe-function 'text-properties-at)] *)
val properties : t -> at:int -> Property.t list

(** [(describe-function 'put-text-property)]
    [(Info-goto-node "(elisp)Changing Properties")]. *)
val set_property
  :  ?start:int (** default is start of [t] *)
  -> ?end_:int (** default is end of [t] *)
  -> t
  -> 'a Property_name.t
  -> 'a
  -> unit

(** [(describe-function 'set-text-properties)]
    [(Info-goto-node "(elisp)Changing Properties")] *)
val set_properties
  :  ?start:int (** default is start of [t] *)
  -> ?end_:int (** default is end of [t] *)
  -> t
  -> Property.t list
  -> unit

(** [(describe-function 'add-text-properties)]
    [(Info-goto-node "(elisp)Changing Properties")] *)
val add_properties
  :  ?start:int (** default is start of [t] *)
  -> ?end_:int (** default is end of [t] *)
  -> t
  -> Property.t list
  -> unit

(** [(describe-function 'add-face-text-property)]
    [(Info-goto-node "(elisp)Changing Properties")] *)
val add_face_properties
  :  ?start:int (** default is start of [t] *)
  -> ?end_:int (** default is end of [t] *)
  -> ?append:bool (** default is false *)
  -> t
  -> spec:Face_spec.t
  -> t

(** [(describe-function 'remove-list-of-text-properties)]
    [(Info-goto-node "(elisp)Changing Properties")] *)
val remove_properties
  :  ?start:int (** default is start of [t] *)
  -> ?end_:int (** default is end of [t] *)
  -> t
  -> Property_name.Packed.t list
  -> unit

(** [(describe-function 'multibyte-string-p)]
    [(Info-goto-node "(elisp)Text Representations")] *)
val is_multibyte : t -> bool

(** [(describe-function 'string-to-multibyte)]
    [(Info-goto-node "(elisp)Converting Representations")] *)
val to_multibyte : t -> t

(** [(describe-function 'string-to-unibyte)]
    [(Info-goto-node "(elisp)Converting Representations")] *)
val to_unibyte_exn : t -> t

(** [(describe-function 'string)] *)
val of_char_array : Char_code.t array -> t

(** [(describe-function 'string-to-vector)] *)
val to_char_array : t -> Char_code.t array

(** [(describe-function 'string-search)] *)
val string_search : needle:string -> t -> int option
