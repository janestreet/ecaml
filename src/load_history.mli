(** [load-history] is an Emacs alist that associates the names of loaded library files
    with the names of the functions and variables they defined, as well as the features
    they provided or required.  This is used by [find-function], [find-variable], and in
    [*Help*] buffers to jump from a symbol to its definition.

    [(Info-goto-node "(elisp)Where Defined")] *)

open! Core_kernel
open! Import

(** [(describe-function 'symbol-file)] *)
val defining_file : Symbol.t -> string option

(** [add_defuns] updates [load-history] with the information in
    [Function.get_and_clear_defuns], which thus makes it possible to jump from a symbol
    defined by Ecaml to the Ecaml source.  Each source-file name is adjusted by:

    {[
      Filename.concat in_dir (String.chop_prefix_exn file ~prefix:chop_prefix)
    ]} *)
val add_defuns
  :  chop_prefix : string
  -> in_dir      : string
  -> unit

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
