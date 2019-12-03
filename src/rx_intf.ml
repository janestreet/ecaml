(** Symbolic construction of emacs regexps.

    [(describe-function 'rx)]

    N.B. These bindings are incomplete only due to laziness (in the technical sense). *)

open! Core_kernel
open! Import

module Char_class = struct
  type t =
    | Chars_in of string
    (** [Chars_in s] matches characters in [s] (e.g., "[:digit:]" matches ':')  *)
    | Range of char * char
    (** [Range (c1, c2)] matches characters between [c1] and [c2] inclusive. *)
  [@@deriving sexp_of]
end

module Start_or_end = struct
  type t =
    | End
    | Start
  [@@deriving sexp_of]
end

module T = struct
  type t =
    | Any_char
    | Any_in of Char_class.t list
    | Exactly of string
    (** [Exactly s] matches exactly the string [s] (e.g., '.' only matches '.') *)
    | Line of Start_or_end.t
    | None_in of Char_class.t list
    | One_or_more of t
    | Or of t list
    | Pattern of string
    (** [Pattern s] interprets [s] as a regexp (e.g., '.' matches any character) *)
    | Point (** Matches zero characters, but only where the point is. *)
    | Repeat of
        { min : int
        ; max : int option
        ; t : t
        }
    | Seq of t list
    | Submatch of t
    (** Text matched by [Submatch] can be retrieved using [Regexp.Last_match.text_exn
        ~subexp:index].

        [index] is the number of parentheses to the left of this submatch in the regexp
        pattern. *)
    | Submatch_n of
        { index : int
        ; t : t
        }
    (** Text matched by [Submatch_n] can be retrieved using [Regexp.Last_match.text_exn
        ~subexp:index].

        [index] must be at least the number of parentheses left of this submatch in the
        regexp pattern. If [index] is greater, it will shadow the submatch that would have
        been assigned that index. If [index] is less, then it is an error to use the
        resulting regexp. *)
    | Zero_or_more of t
    | Zero_or_one of t
  [@@deriving sexp_of]
end

module type Rx = sig
  module Char_class : module type of struct
    include Char_class
  end

  module Start_or_end : module type of struct
    include Start_or_end
  end

  include module type of T with type t = T.t

  val pattern : t -> string
end
