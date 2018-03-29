(** A "regular expression", or "regexp" for short, is a pattern that denotes a (possibly
    infinite) set of strings.

    [(Info-goto-node "(elisp)Regular Expressions")] *)

open! Core_kernel
open! Import

include Value.Subtype

(** [Rx.t] is the preferred way to construct regexps, especially when the alternative
    involves string manipulation in OCaml. *)
val of_rx : Rx.t -> t

val match_anything : t
val match_nothing  : t

(** [(Info-goto-node "(elisp)Syntax of Regexps")] *)
val of_pattern : string -> t

val to_pattern : t -> string

val any : t list -> t

val any_pattern : string list -> t

(** [quote string] matches [string] and nothing else.
    [(describe-function 'regexp-quote)]
    [(Info-goto-node "(elisp)Regexp Functions")] *)
val quote : string -> t

(** [any_quote strings] matches every string in [strings], and nothing else.
    [(describe-function 'regexp-opt)]
    [(Info-goto-node "(elisp)Regexp Functions")] *)
val any_quote : string list -> t

(** Supplying [~update_last_match:true] to a searching function causes Emacs to keep track
    of the "last match", i.e. the start and end positions of the segments of text found
    during the search.  One can access parts of the last match via the [Last_match]
    functions.  [subexp] is one based.
    [(Info-goto-node "(elisp)Match Data")] *)
module Last_match : sig
  type t [@@deriving sexp_of]

  (** [(describe-function 'match-data)]
      [(Info-goto-node "(elisp)Entire Match Data")] *)
  val get_exn : unit -> t

  (** [(describe-function 'set-match-data)]
      [(Info-goto-node "(elisp)Entire Match Data")] *)
  val set : t -> unit

  (** [(describe-function 'save-match-data)]
      [(Info-goto-node "(elisp)Saving Match Data")] *)
  val save : (unit -> 'a) -> 'a

  (** [text_exn] returns the text of the last match, or the [subexp]'th parenthesized
      subexpression.
      [(describe-function 'match-string)]
      [(Info-goto-node "(elisp)Simple Match Data")] *)
  val text_exn
    :  ?subexp          : int   (** default is entire match *)
    -> ?text_properties : bool  (** default is [false] *)
    -> unit
    -> Text.t

  (** [start_exn] returns the index of the start of the last match, or the [subexp]'th
      parenthesized subexpression.
      [(describe-function 'match-beginning)]
      [(Info-goto-node "(elisp)Simple Match Data")] *)
  val start_exn
    :  ?subexp : int  (** default is entire match *)
    -> unit
    -> int

  (** [end_exn] returns the index after the end of the last match, or the [subexp]'th
      parenthesized subexpression.
      [(describe-function 'match-end)]
      [(Info-goto-node "(elisp)Simple Match Data")] *)
  val end_exn
    :  ?subexp : int  (** default is entire match *)
    -> unit
    -> int

  module Private : sig
    module Location : sig
      type t =
        | Buffer of Buffer.t
        | No_match
        | Text of Text.t
      [@@deriving sexp_of]

      val last : t ref
    end
  end
end

(** [match_ t text] finds the first match of [t] in [text], returns the index of the start
    of the match.
    [(describe-function 'string-match)]
    [(describe-function 'string-match-p)]
    [(Info-goto-node "(elisp)Regexp Search")] *)
val match_
  :  ?start             : int   (** default is 0 *)
  -> ?update_last_match : bool  (** default is [false] *)
  -> t
  -> Text.t
  -> int option

(** [does_match t text] is [is_some (match_ t text)] *)
val does_match
  :  ?start             : int   (** default is 0 *)
  -> ?update_last_match : bool  (** default is [false] *)
  -> t
  -> Text.t
  -> bool
