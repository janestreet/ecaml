(** [(Info-goto-node "(elisp)Customization")] *)

open! Core_kernel
open! Import

module Group : sig
  type t [@@deriving sexp_of]

  val of_string : string -> t
  val to_string : t -> string
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
end

(** [(describe-function 'defcustom)]
    [(Info-goto-node "(elisp)Variable Definitions")] *)
val defcustom
  :  Source_code_position.t
  -> Symbol.t
  -> Type.t
  -> docstring      : string
  -> group          : Group.t
  -> standard_value : Value.t
  -> unit
