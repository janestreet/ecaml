(** A character code is an integer representing a character.  Emacs character codes are a
    superset of the Unicode standard.

    [(Info-goto-node "(elisp)Character Type")]
    [(Info-goto-node "(elisp)Character Codes")] *)

open! Core_kernel
open! Import

include Value.Subtype

include Equal.S with type t := t

val of_int_exn : int -> t

val to_int : t -> int

(** [of_char_exn char] raises if [char] isn't ASCII, i.e. if [Char.to_int char >= 128]. *)
val of_char_exn : char -> t

(** zero *)
val min_value : t

(** [(describe-function 'max-char)]
    [(Info-goto-node "(elisp)Character Codes")] *)
val max_value : t
