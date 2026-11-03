(** [load-history] is an Emacs alist that associates the names of loaded library files
    with the names of the functions and variables they defined, as well as the features
    they provided or required. This is used by [find-function], [find-variable], and in
    [*Help*] buffers to jump from a symbol to its definition.

    [(Info-goto-node "(elisp)Where Defined")] *)

open! Core
open! Import0

(** [(describe-function 'symbol-file)] *)
val defining_file : Symbol.t -> string option

(** [Entry] defines the various kinds of entries stored in [load-history]. *)
module Entry : sig
  type t =
    | Autoload of Symbol.t
    | Face of Symbol.t
    | Fun of Symbol.t
    | Previously_an_autoload of Symbol.t
    | Provide of Symbol.t
    | Require of Symbol.t
    | Var of Symbol.t
  [@@deriving sexp_of]

  val to_value : t -> Value.t
end

(** [add_entry] is called by ([defcustom], [defun], [defvar]) and adds the entry to a
    [list ref], for later use by [Find_function.update_emacs_with_entries]. *)
val add_entry : Source_code_position.t -> Entry.t -> unit

(** [take_entries] returns the entries recorded by [add_entry] since the last call to
    [take_entries]. *)
val take_entries : unit -> (Source_code_position.t * Entry.t) list

(** [Type] represents the type of symbol being searched for, corresponding to the [TYPE]
    argument of [find-function-search-for-symbol]. *)
module Type : sig
  type t =
    | Face
    | Fun
    | Var
  [@@deriving enumerate, sexp_of]

  include Valueable.S with type t := t
end

(** [location_exn symbol type_] is the position passed to [add_entry] for the definition
    of [symbol] as a [type_]. Unlike [take_entries], this remembers all definitions. *)
val location_exn : Symbol.t -> Type.t -> Source_code_position.t
