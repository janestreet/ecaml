(** Makes [(describe-function 'find-function)] and friends able to jump from a symbol to
    the code that defines it, even when that code is Ecaml. *)

open! Core
open! Import

(** [update_emacs_with_entries] updates [load-history] with the information supplied to
    [Load_history.add_entry], which make it possible to, within Emacs, jump from a symbol
    defined by Ecaml to the Ecaml source. Each source-file name is adjusted by:

    {[
      Filename.concat in_dir (String.chop_prefix_exn file ~prefix:chop_prefix)
    ]}

    It also sets each symbol's [find-function-type-alist] property so that [find-function]
    moves point to the exact definition site within that file, rather than searching the
    OCaml source for an Elisp definition form. *)
val update_emacs_with_entries : chop_prefix:string -> in_dir:string -> unit
