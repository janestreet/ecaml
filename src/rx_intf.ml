open! Core_kernel
open! Import

(** Symbolic construction of emacs regexps.

    [(describe-function 'rx)]

    N.B. These bindings are incomplete only due to laziness (in the technical sense).
*)

module Char_class = struct
  type t =
    (** [Chars_in s] matches characters in [s] (e.g., "[:digit:]" matches ':')  *)
    | Chars_in of string
    (** [Range (c1, c2)] matches characters between [c1] and [c2] inclusive. *)
    | Range of char * char
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
    | Any_in of Char_class.t list
    (** [Exactly s] matches exactly the string [s] (e.g., '.' only matches '.') *)
    | Exactly of string
    | Line of Start_or_end.t
    | None_in of Char_class.t list
    | Or of t list
    (** [Pattern s] interprets [s] as a regexp (e.g., '.' matches any character) *)
    | Pattern of string
    (** Matches zero characters, but only where the point is. *)
    | Point
    | Repeat of { min : int; max : int option; t : t }
    | Seq of t list
    (** Text matched by [Submatch] can be retrieved using [Regexp.Last_match.text_exn
        ~subexp:index].

        [index] is the number of parentheses to the left of this submatch in the regexp
        pattern. *)
    | Submatch of t
    (** Text matched by [Submatch_n] can be retrieved using [Regexp.Last_match.text_exn
        ~subexp:index].

        [index] must be at least the number of parentheses left of this submatch in the
        regexp pattern. If [index] is greater, it will shadow the submatch that would have
        been assigned that index. If [index] is less, then it is an error to use the
        resulting regexp. *)
    | Submatch_n of { index : int; t : t }
  [@@deriving sexp_of]

end

module type Rx = sig
  module Char_class   : module type of struct include Char_class   end
  module Start_or_end : module type of struct include Start_or_end end

  include module type of T with type t = T.t

  val pattern : t -> string

end
