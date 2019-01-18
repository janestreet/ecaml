(** [(Info-goto-node "(elisp)Customization")] *)

open! Core_kernel
open! Import

module type Enum_arg = sig
  type t [@@deriving compare, enumerate, sexp_of]


  val docstring : t -> string
  val standard_value : t
  val to_symbol : t -> Symbol.t
end

module type Enum = sig
  type t

  include Valueable.S with type t := t

  val var : t Var.t
  val initialize_defcustom : unit -> unit
end

module type Customization = sig
  module Group : sig
    type t [@@deriving sexp_of]

    include Valueable.S with type t := t

    val emacs : t

    (** Define a new customization group.

        [(describe-function 'defgroup)]
        [(Info-goto-node "(elisp)Group Definitions")] *)
    val defgroup
      :  Symbol.t
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
      | List of t list
      | Number
      | Option of string * t
      | Plist
      | Radio of t list
      | Regexp
      | Repeat of t
      | Set of t list
      | Sexp
      | String
      | Symbol
      | Tagged_string of string
      | Variable
      | Vector of t list
    [@@deriving sexp_of]

    val enum : 'a list -> ('a -> Value.t) -> t
  end

  (** [(describe-function 'defcustom)]
      [(Info-goto-node "(elisp)Variable Definitions")] *)
  val defcustom
    :  ?show_form:bool (** default is [false] *)
    -> Symbol.t
    -> Source_code_position.t
    -> docstring:string
    -> group:Group.t
    -> type_:'a Value.Type.t
    -> customization_type:Type.t
    -> standard_value:'a
    -> unit
    -> 'a Var.t

  (** [(describe-function 'customize-variable)]
      [(Info-goto-node "(emacs)Specific customization")] *)
  val customize_variable : Symbol.t -> unit

  (** [(describe-function 'customize-group)]
      [(Info-goto-node "(emacs)Specific customization")] *)
  val customize_group : Group.t -> unit

  module Enum : sig
    module type Arg = Enum_arg
    module type S = Enum

    val make
      :  Symbol.t
      -> Source_code_position.t
      -> (module Arg with type t = 'a)
      -> docstring:string
      -> group:Group.t
      -> (module S with type t = 'a)
  end

  module Private : sig
    val all_defcustom_symbols : unit -> Symbol.t list
    val all_defgroups : unit -> Group.t list
  end
end
