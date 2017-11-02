(** A syntax table specifies the syntactic textual function of each character.  This
    information is used by the parsing functions, the complex movement commands, and
    others to determine where words, symbols, and other syntactic constructs begin and
    end.

    [(Info-goto-node "(elisp)Syntax Tables")] *)

open! Core_kernel
open! Import

include Value.Subtype

include Equal.S with type t := t

(** [(describe-function 'standard-syntax-table)]
    [(Info-goto-node "(elisp)Standard Syntax Tables")] *)
val standard : t

(** [(describe-function 'make-syntax-table)]
    [(Info-goto-node "(elisp)Syntax Table Functions")] *)
val create : ?parent:t -> unit -> t

(** [(describe-function 'copy-syntax-table)]
    [(Info-goto-node "(elisp)Syntax Table Functions")] *)
val copy : t -> t

(** [(Info-goto-node "(elisp)Syntax Class Table")]*)
module Class : sig
  type t =
    | Char_quote
    | Close_paren
    | Comment_end
    | Comment_start
    | Escape
    | Expression_prefix
    | Generic_comment_delimiter
    | Generic_string_delimiter
    | Inherit_standard
    | Open_paren
    | Paired
    | Punctuation
    | String_quote
    | Symbol_constitutent
    | Whitespace
    | Word_constituent
  [@@deriving enumerate, sexp_of]

  include Equal.S          with type t := t
  include Hashable.S_plain with type t := t

  val of_char_code_exn : Char_code.t -> t

  val to_string : t -> string
end

(** [(Info-goto-node "(elisp)Syntax Flags")]*)
module Flag : sig
  type t =
    | Alternative_comment
    | Commend_end_first_char
    | Comment_end_second_char
    | Comment_start_first_char
    | Comment_start_second_char
    | Nested
    | Prefix_char
  [@@deriving enumerate, sexp_of]
end

module Descriptor : sig
  type t = Class.t * Flag.t list
  [@@deriving sexp_of]
end

(** [(describe-function 'modify-syntax-entry)]
    [(Info-goto-node "(elisp)Syntax Table Functions")] *)
val set      : t -> Char_code.t -> Class.t -> Flag.t list -> unit
val set_char : t -> char        -> Class.t -> Flag.t list -> unit
