(** Just as you can run a compiler from Emacs and then visit the lines with compilation
    errors, you can also run ‘grep’ and then visit the lines on which matches were found.
    This works by treating the matches reported by ‘grep’ as if they were errors.

    [(Info-goto-node "(emacs)Grep Searching")] *)

open! Core_kernel
open! Import

(** [(describe-function 'grep))]
    [(Info-goto-node "(emacs)Grep Searching")] *)
val grep : command : string -> unit
