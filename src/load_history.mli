(** [load-history] is an Emacs alist that associates the names of loaded library files
    with the names of the functions and variables they defined, as well as the features
    they provided or required.  This is used by [find-function], [find-variable], and in
    [*Help*] buffers to jump from a symbol to its definition.

    [(Info-goto-node "(elisp)Where Defined")] *)

open! Core_kernel
open! Import0

(** [(describe-function 'symbol-file)] *)
val defining_file : Symbol.t -> string option

(** [Entry] defines the various kinds of entries stored in [load-history]. *)
module Entry : sig
  type t =
    | Autoload               of Symbol.t
    | Face                   of Face.t
    | Fun                    of Symbol.t
    | Previously_an_autoload of Symbol.t
    | Provide                of Symbol.t
    | Require                of Symbol.t
    | Var                    of Symbol.t
  [@@deriving sexp_of]

  val to_value : t -> Value.t
end

(** [add_entry] is called by ([defcustom], [defun], [defvar]) and adds the entry to a
    [list ref], for later use by [update_emacs_with_entries]. *)
val add_entry : Source_code_position.t -> Entry.t -> unit

(** [update_emacs_with_entries] updates [load-history] with the information supplied to
    [add_entry], which make it possible to, within Emacs, jump from a symbol defined by
    Ecaml to the Ecaml source.  Each source-file name is adjusted by:

    {[
      Filename.concat in_dir (String.chop_prefix_exn file ~prefix:chop_prefix)
    ]} *)
val update_emacs_with_entries
  :  chop_prefix : string
  -> in_dir      : string
  -> unit

module Type : sig
  type t =
    | Fun
    | Var
  [@@deriving sexp_of]
end

val location_exn : Symbol.t -> Type.t -> Source_code_position.t
