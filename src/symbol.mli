(** An Emacs symbol, i.e., an Emacs value that satisfies [Value.is_symbol].

    [(Info-goto-node "(elisp)Symbols")] *)

open! Core_kernel
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

(** [(describe-function 'make-symbol)]. *)
val create : name:string -> t

(** [(describe-function 'gensym)]. *)
val gensym : ?prefix:string -> unit -> t

(** [(describe-function 'fset)]. *)
val set_function : t -> Value.t -> unit

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

      Most functions use (1), but some use (2), e.g. a [defalias] like [abbrev-get]
      or an advised function like [next-error]. *)
  val function_documentation : Value.t t

  val variable_documentation : Value.t t

  (** If the disabled property is true for a symbol [x], and the user has not disabled the
      feature, and the user tries to do the command [x], they will get a warning asking if
      they really want to do the command (e.g. narrow-to-region).

      See [(Info-goto-node "(emacs)Disabling")] *)
  val function_disabled : Bool.t t
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
