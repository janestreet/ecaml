open! Core_kernel
open! Import

(** [(Info-goto-node "(emacs) Bookmarks")] *)

type t [@@deriving sexp_of]

include Comparator.S with type t := t
include Stringable.S with type t := t
include Valueable.S with type t := t

module Property : sig
  type 'a t =
    { symbol : Symbol.t
    ; type_ : 'a Value.Type.t
    }
  [@@deriving sexp_of]

  val create : string -> 'a Value.Type.t -> 'a t

  module And_value : sig
    type 'a property := 'a t
    type t = T : 'a property * 'a -> t
  end

  val and_value : 'a t -> 'a -> And_value.t

  (** {2 Standard properties}

      [(describe-variable 'bookmark-alist)] documents most of them.

      [front_context_string] and [rear_context_string] are meant to handle changes to the
      file. The [bookmark-default-handler] function first jumps to [position], then
      searches forward for [front_context_string], then searches backwards for
      [rear_context_string]. *)

  val annotation : string t
  val filename : Filename.t t
  val front_context_string : string t

  (** Typical use is to [defun] a handler and set [handler] to [Function.of_symbol].

      The property has type [Function.t t] so that we can deserialize values like [(lambda
      ...)] set from elisp. *)
  val handler : Function.t t

  val position : Position.t t
  val rear_context_string : string t
end

module Record : sig
  (** The data side of [bookmark-alist] *)

  type t = Value.t Map.M(Symbol.Compare_name).t [@@deriving sexp_of]

  include Valueable.S with type t := t

  (** [get t key] returns [None] if [key] is not in [t], and raises if the value has an
      unexpected type. *)
  val get : t -> 'a Property.t -> 'a option

  val get_exn : t -> 'a Property.t -> 'a

  (** [set t key data] silently overrides any existing value of [key] in [t]. *)
  val set : t -> 'a Property.t -> 'a -> t

  (** It is an error to repeat properties. *)
  val create : Property.And_value.t list -> t
end

(** [set m t a ~no_overwrite:true] means, let [a] shadow any previous location
    bookmarked as [t]. If the user removes this bookmark, the previous bookmark of this
    name will be restored. *)
val set : t -> Record.t -> no_overwrite:bool -> unit

val param : (Record.t -> 'a) -> 'a Defun.t

module Make_record_function : sig
  (** [(describe-variable 'bookmark-make-record-function)] *)
  module Return_type : sig
    type t =
      { record : Record.t
      ; suggested_bookmark_name : string option
      }

    include Valueable.S with type t := t
  end

  type t = unit -> Return_type.t

  (** The symbol should be a function of type [t]. **)
  val in_buffer : Symbol.t Buffer_local.t

  (** [(describe-function 'bookmark-make-record-default)] *)
  val default
    :  ?no_context:bool
    -> ?no_file:bool
    -> ?position:Position.t
    -> unit
    -> Return_type.t
end
