(** Just as you can run a compiler from Emacs and then visit the lines with compilation
    errors, you can also run ‘grep’ and then visit the lines on which matches were found.
    This works by treating the matches reported by ‘grep’ as if they were errors.

    [(Info-goto-node "(emacs)Grep Searching")] *)

open! Core_kernel
open! Import
include Major_mode.S

(** [(describe-function 'grep))]
    [(Info-goto-node "(emacs)Grep Searching")] *)
val grep : command:string -> unit

module Save_buffers : sig
  type t =
    | Ask
    | False
    | True
  [@@deriving sexp_of]
end

(** [(describe-variable 'grep-save-buffers)] *)
val save_buffers : Save_buffers.t Customization.t
