(** An Emacs symbol, i.e., an Emacs value that satisfies [Value.is_symbol].

    [(Info-goto-node "(elisp)Symbols")] *)

open! Core
open! Import0

include module type of struct
  include Ecaml_value.Symbol
end

type symbol := t

(** Accessors *)

(** [(describe-function 'symbol-name)    ] *)
val name : t -> string

(** [(describe-function 'symbol-function)] *)
val function_exn : t -> Value.t

(** [(describe-function 'fboundp)] *)
val function_is_defined : t -> bool

val compare_name : t -> t -> int

(** [(describe-function 'gensym)]. *)
val gensym : ?prefix:string -> unit -> t

(** [Automatic_migration] supports migrating from one symbol-naming convention to another.
    Use [Automatic_migration.add] to add a function that maps names from the old
    convention to the new. Once you've done that, the following functions will
    automatically migrate their symbol argument, and and an obsolete alias pointing from
    the old name to the new.

    - defcustom
    - defun
    - define_minor_mode
    - defvar *)
module Automatic_migration : sig
  module New : sig
    type nonrec t =
      { new_ : t
      ; since : string
      }
    [@@deriving sexp_of]
  end

  val add : (old:t -> New.t option) -> unit
  val migrate : old:t -> New.t option
end

module Property : sig
  type 'a t [@@deriving sexp_of]

  (** A property of an Emacs symbol.

      [(Info-goto-node "(elisp)Symbol Properties")] *)

  (** Create a property named by the given symbol. *)
  val create : symbol -> 'a Value.Type.t -> 'a t

  (** [(describe-function 'get)] *)
  val get : 'a t -> symbol -> 'a option

  (** [(describe-function 'get)] *)
  val get_exn : 'a t -> symbol -> 'a

  (** [(describe-function 'put)] *)
  val put : 'a t -> symbol -> 'a -> unit

  (** According to [(Info-goto-node "(elisp)Documentation Basics")], there are two places
      where one can put a documentation string:

      1. in the lambda after the arg list, or
      2. in the function symbol's function-documentation property

      Most functions use (1), but some use (2), e.g. a [defalias] like [abbrev-get] or an
      advised function like [next-error]. *)
  val function_documentation : Value.t t

  val variable_documentation : Value.t t

  (** If a command has multiple bindings, [(describe-function 'substitute-command-keys)]
      normally uses the first one it finds. You can specify one particular key binding by
      a value for this property to the command, like this.

      This property also affects the binding shown in menu items. The property is ignored
      if it specifies a key binding that the command does not actually have.

      See [(Info-goto-node "(elisp) Keys in Documentation")] *)
  val advertised_binding : Key_sequence0.t t
end

module type Subtype = sig
  type t

  val of_symbol_exn : symbol -> t
  val to_symbol : t -> symbol
  val of_value_exn : Value.t -> t
  val to_value : t -> Value.t
end

module Make_subtype (Arg : sig
    type t [@@deriving enumerate, sexp_of]

    val module_name : string
    val to_symbol : t -> symbol
  end) : Subtype with type t := Arg.t

module Compare_name : sig
  type t = symbol [@@deriving compare, sexp_of]

  include Comparator.S with type t := t
end
