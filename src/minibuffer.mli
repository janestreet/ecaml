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

(** [(describe-function 'y-or-n-p)]
    [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val y_or_n : prompt : string -> bool

module Y_or_n_with_timeout : sig
  type 'a t = Y | N | Timeout of 'a [@@deriving sexp_of]
end

(** [(describe-function 'y-or-n-p-with-timeout)]
    [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val y_or_n_with_timeout
  :  prompt : string
  -> timeout : Time_ns.Span.t * 'a
  -> 'a Y_or_n_with_timeout.t

(** [(describe-function 'yes-or-no-p)]
    [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val yes_or_no : prompt : string -> bool

(** [(describe-variable 'minibuffer-exit-hook)]
    [(Info-goto-node "(elisp)Minibuffer Misc")] *)
val exit_hook : Hook.normal Hook.t

(** [(describe-variable 'minibuffer-setup-hook)]
    [(Info-goto-node "(elisp)Minibuffer Misc")] *)
val setup_hook : Hook.normal Hook.t
