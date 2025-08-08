(** [(Info-goto-node "(elisp)Customization")] *)

open! Core
open! Import

module type Enum_arg = sig
  type t

  include Enum.S with type t := t

  val docstring : t -> string option
end

module type Enum_arg_to_symbol = sig
  include Enum_arg

  val to_symbol : t -> Symbol.t
end

module type Customization = sig
  module Group : sig
    type t [@@deriving sexp_of]

    include Valueable.S with type t := t

    val emacs : t
    val ecaml : t

    (** Define a new customization group.

        [(describe-function 'defgroup)] [(Info-goto-node "(elisp)Group Definitions")] *)
    val defgroup
      :  string
      -> Source_code_position.t
      -> docstring:string
      -> parents:t list
      -> t

    (** Reference an already-existing customization group. *)
    val of_string : string -> t

    val to_string : t -> string
    val to_symbol : t -> Symbol.t
  end

  (** [(Info-goto-node "(elisp)Customization Types")] *)
  module Type : sig
    type t =
      | Alist of t * t
      | Boolean
      | Character
      | Choice of t list
      | Coding_system
      | Color
      | Cons of t * t
      | Const of Value.t
      | Directory
      | Existing_file
      | Face
      | File
      | Float
      | Function
      | Group of t
      | Hook
      | Integer
      | Key_sequence
      | List of t list
      | Number
      | Option of
          { doc_for_none : string
          ; t : t
          }
      | Plist
      | Radio of t list
      | Regexp
      | Repeat of t
      | Set of t list
      | Sexp
      | String
      | Symbol
      | Tag of string * t
      | Variable
      | Vector of t list
    [@@deriving sexp_of]

    val enum : 'a list -> ('a -> Value.t) -> t
    [@@deprecated "[since 2024-07] Use [defcustom_enum] instead."]
  end

  type 'a t [@@deriving sexp_of]

  val value : 'a t -> 'a
  val set_value : 'a t -> 'a -> unit
  val set_value_temporarily : 'a t -> 'a -> f:(unit -> 'b) -> 'b
  val var : 'a t -> 'a Var.t
  val symbol : _ t -> Symbol.t
  val name : _ t -> string
  val standard_value : 'a t -> 'a

  (** [(describe-function 'defcustom)] [(Info-goto-node "(elisp)Variable Definitions")]

      [on_set], if supplied, is called when the user attempts to set the customization,
      either via the customization interface or via [custom-set-variables]. [on_set] can
      validate the input and perform other side effects needed to keep OCaml data
      structures in sync with the setting. If [on_set] raises, the customization is not
      set. *)
  val defcustom
    :  Symbol.t
    -> Source_code_position.t
    -> docstring:string
    -> group:Group.t
    -> type_:'a Value.Type.t
    -> customization_type:Type.t
    -> standard_value:'a
    -> ?on_set:('a -> unit)
    -> unit
    -> 'a t

  module Wrap : Var.Wrap with type 'a t := 'a t

  (** [(describe-function 'customize-variable)]
      [(Info-goto-node "(emacs)Specific customization")] *)
  val customize_variable : Symbol.t -> unit

  (** [(describe-function 'customize-group)]
      [(Info-goto-node "(emacs)Specific customization")] *)
  val customize_group : Group.t -> unit

  module type Enum_arg = Enum_arg
  module type Enum_arg_to_symbol = Enum_arg_to_symbol

  val defcustom_enum
    :  Symbol.t
    -> Source_code_position.t
    -> (module Enum_arg with type t = 'a)
    -> docstring:string
    -> group:Group.t
    -> standard_value:'a
    -> ?on_set:('a -> unit)
    -> unit
    -> 'a t

  (** Like [defcustom_enum], but allows the caller to specify a custom [to_symbol]
      function. *)
  val defcustom_enum_with_to_symbol
    :  Symbol.t
    -> Source_code_position.t
    -> (module Enum_arg_to_symbol with type t = 'a)
    -> docstring:string
    -> group:Group.t
    -> standard_value:'a
    -> ?on_set:('a -> unit)
    -> unit
    -> 'a t

  module Private : sig
    val all_defcustom_symbols : unit -> Symbol.t list
    val all_defgroups : unit -> Group.t list
  end
end
