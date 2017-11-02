(** Loading a file of Lisp code means bringing its contents into the Lisp environment in
    the form of Lisp objects.  Emacs finds and opens the file, reads the text, evaluates
    each form, and then closes the file.

    [(Info-goto-node "(elisp)Loading")] *)

open! Core_kernel
open! Import

(** [(describe-function 'load)]
    [(Info-goto-node "(elisp)How Programs Do Loading")] *)
val load
  :  ?message : bool  (** default is [true] *)
  -> string
  -> unit

(** [(describe-variable 'load-path)]
    [(Info-goto-node "(elisp)Library Search")] *)
val path : unit -> string list
