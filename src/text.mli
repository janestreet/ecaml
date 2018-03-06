(** Emacs text, which is a sequence of characters, along with text properties associated
    with each character.

    [(Info-goto-node "(elisp)Text")] *)

open! Core_kernel
open! Import

include Value.Subtype

module Compare_as_string : sig
  include Comparable.S with type t = t
  include Sexpable.S with type t := t
end

(** [(describe-function 'aref)]
    [(Info-goto-node "(elisp)String Basics")] *)
val char_code : t -> int -> Char_code.t

(** [(describe-function 'aset)]
    [(Info-goto-node "(elisp)String Basics")] *)
val set_char_code : t -> int -> Char_code.t -> unit

val of_utf8_bytes : string -> t
val to_utf8_bytes : t -> string

(** [length t] returns the number of characters in [t]. *)
val length : t -> int

(** [num_bytes t] returns the number of bytes in the representation of [t], which, if
    [is_multibyte t], can be greater than [length t].
    [(describe-function 'string-bytes)] *)
val num_bytes : t -> int

(** [(describe-function 'concat)] *)
val concat : t list -> t

(** [(Info-goto-node "(elisp)Special Properties")] *)
module Face_spec : sig
  module One : sig
    type t =
      | Attributes of Face.Attribute_and_value.t list
      | Face of Face.t
    [@@deriving sexp_of]

    val of_value_exn : Value.t -> t
    val to_value : t -> Value.t
  end

  type t = One.t list [@@deriving sexp_of]

  val of_value_exn : Value.t -> t
  val to_value : t -> Value.t

  (** [normalize] sorts [t]; it is used to get consistent ordering when comparing
      [Ansi_color] to Elisp's [ansi-color] functionality. *)
  val normalize : t -> t
end

(** [(Info-goto-node "(elisp)Text Properties")] *)
module Property_name : sig
  type 'a t [@@deriving sexp_of]

  (** [(Info-goto-node "(elisp)Special Properties")] *)
  val face           : Face_spec.t t
  val font_lock_face : Face_spec.t t

  val name          : _ t -> Symbol.t
  val name_as_value : _ t -> Value.t

  val to_value : 'a t -> 'a -> Value.t

  module type S = sig
    module Property_value : sig
      type t [@@deriving sexp_of]
      val of_value_exn : Value.t -> t
      val to_value : t -> Value.t
    end
    val name : Symbol.t
  end

  (** [create_and_register (module M)] creates a new property name, and registers it with
      state in [Property_name] to enable conversion from an Elisp property list containing
      [M.name] and the associated [Value.t] to a [Property.t]. *)
  val create_and_register : (module S with type Property_value.t = 'a) -> 'a t

  module Packed : sig
    type 'a property_name
    type t = T : _ property_name -> t
    [@@deriving sexp_of]

    val name : t -> Symbol.t
  end with type 'a property_name := 'a t
end

(** [(Info-goto-node "(elisp)Text Properties")] *)
module Property : sig
  type t = T : 'a Property_name.t * 'a -> t
  [@@deriving sexp_of]

  val to_property_list : t list -> Value.t list
end

(** [(describe-function 'propertize)]
    [(Info-goto-node "(elisp)Changing Properties")] *)
val propertize : t -> Property.t list -> t

(** [property_value t ~at property_name] returns the value of [property_name] for the
    character after [at].  [at] is a zero-based index into [t]; [property_value] raises
    unless [0 <= at <= length t] (allowing [length] is different than OCaml).
    [(describe-function 'get-text-property)]
    [(Info-goto-node "(elisp)Examining Properties")] *)
val property_value : t -> at:int -> 'a Property_name.t -> 'a option

(** [(describe-function 'text-properties-at)] *)
val properties : t -> at:int -> Property.t list

(** [(describe-function 'put-text-property)]
    [(Info-goto-node "(elisp)Changing Properties")]. *)
val set_property
  :  ?start : int (** default is start of [t] *)
  -> ?end_  : int (** default is end of [t] *)
  -> t
  -> 'a Property_name.t
  -> 'a
  -> unit

(** [(describe-function 'set-text-properties)]
    [(Info-goto-node "(elisp)Changing Properties")] *)
val set_properties
  :  ?start : int (** default is start of [t] *)
  -> ?end_  : int (** default is end of [t] *)
  -> t
  -> Property.t list
  -> unit

(** [(describe-function 'add-text-properties)]
    [(Info-goto-node "(elisp)Changing Properties")] *)
val add_properties
  :  ?start : int (** default is start of [t] *)
  -> ?end_  : int (** default is end of [t] *)
  -> t
  -> Property.t list
  -> unit

(** [(describe-function 'remove-list-of-text-properties)]
    [(Info-goto-node "(elisp)Changing Properties")] *)
val remove_properties
  :  ?start : int (** default is start of [t] *)
  -> ?end_  : int (** default is end of [t] *)
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
