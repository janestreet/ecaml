(** Just as you can run a compiler from Emacs and then visit the lines with compilation
    errors, you can also run ‘grep’ and then visit the lines on which matches were found.
    This works by treating the matches reported by ‘grep’ as if they were errors.

    [(Info-goto-node "(emacs)Grep Searching")] *)

open! Core
open! Import

val major_mode : Major_mode.t

(** Run [command] and report its output in a grep-mode buffer. *)
val grep : command:string -> unit

(** [(describe-variable 'grep-last-buffer)] *)
val last_buffer : Buffer.t option Var.t
