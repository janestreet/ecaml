(** A "position" is the index of a character in the text of a buffer.  More precisely, a
    position identifies the place between two characters (or before the first character,
    or after the last character), so we can speak of the character before or after a given
    position.  However, we often speak of the character "at" a position, meaning the
    character after that position.

    Positions are usually represented as integers starting from 1, but can also be
    represented as "markers"--special objects that relocate automatically when text is
    inserted or deleted so they stay with the surrounding characters.

    [(Info-goto-node "(elisp)Positions")]. *)

open! Core_kernel
open! Import

include Value.Subtype

include Comparable.S_plain with type t := t

val of_int_exn : int -> t
val to_int : t -> int

val add : t -> int -> t
val sub : t -> int -> t
val diff : t -> t -> int
