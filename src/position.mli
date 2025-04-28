(** A "position" is the index of a character in the text of a buffer. More precisely, a
    position identifies the place between two characters (or before the first character,
    or after the last character), so we can speak of the character before or after a given
    position. However, we often speak of the character "at" a position, meaning the
    character after that position.

    Positions are usually represented as integers starting from 1, but can also be
    represented as "markers"--special objects that relocate automatically when text is
    inserted or deleted so they stay with the surrounding characters.

    [(Info-goto-node "(elisp)Positions")]. *)

open! Core
open! Import0
include Value.Subtype
include Comparable.S_plain with type t := t

val of_int_exn : int -> t
val to_int : t -> int

(** [(describe-function 'position-bytes)].
    [(Info-goto-node "(elisp)Text Representations")] *)
val to_byte_position : t -> int

(** [(describe-function 'byte-to-position)].
    [(Info-goto-node "(elisp)Text Representations")] *)
val of_byte_position : int -> t

val add : t -> int -> t
val sub : t -> int -> t
val diff : t -> t -> int
