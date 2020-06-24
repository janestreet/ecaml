(** A [Modified_tick.t] is an integer value of one of the two modification counters that
    Emacs maintains for each buffer:

    - [buffer-modified-tick], which increments every time a buffer is modified.
    - [buffer-chars-modified-tick], which increments every time characters
      are inserted or deleted in the buffer.

    [(Info-goto-node "(elisp)Buffer Modification")] *)

open! Core_kernel
open! Import0

type t [@@deriving sexp_of]

include Comparable.S with type t := t
include Hashable.S with type t := t
include Valueable.S with type t := t
