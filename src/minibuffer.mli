(** A minibuffer is a special buffer that Emacs commands use to read arguments more
    complicated than the single numeric prefix argument.  These arguments include file
    names, buffer names, and command names (as in [M-x]).  The minibuffer is displayed on
    the bottom line of the frame, in the same place as the echo area, but only while it is
    in use for reading an argument.

    [(Info-goto-node "(elisp)Minibuffers")] *)

open! Core_kernel
open! Import

(** [(describe-function 'read-from-minibuffer)]
    [(Info-goto-node "(elisp)Text from Minibuffer")] *)
val read_from
  :  ?default_value    : string
  -> ?history_list     : Symbol.t
  -> ?history_list_pos : int
  -> ?initial_contents : string
  -> unit
  -> prompt : string
  -> string
