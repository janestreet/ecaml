(** [(Info-goto-node "(emacs)Comments")] *)

(** Note: these are functions rather than [Var.t]s because per-buffer initialization is
    required before these variables are known to be correct. *)

(** [(describe-variable 'comment-start)]
    [(Info-goto-node "(emacs)Options for Comments")] *)
val start : unit -> string

(** [(describe-variable 'comment-start-skip)]
    [(Info-goto-node "(emacs)Options for Comments")] *)
val start_regexp : unit -> Regexp.t

(** [(describe-variable 'comment-end)]
    [(Info-goto-node "(emacs)Options for Comments")] *)
val end_ : unit -> string

(** [(describe-variable 'comment-end-skip)]
    [(Info-goto-node "(emacs)Options for Comments")] *)
val end_regexp : unit -> Regexp.t

(** [(describe-variable 'comment-multi-line)]
    [(Info-goto-node "(emacs)Options for Comments")] *)
val multi_line : unit -> bool

val set_current_buffer_options
  :  start:string
  -> end_:string
  -> is_multi_line:bool
  -> unit

(** Move the point to the beginning of the current comment, and return the position of the
    comment starter.

    [(describe-function 'comment-beginning)] *)
val beginning : unit -> Position.t option

(** Assuming the point is currently in a comment, move to the end of its content. *)
val goto_end_exn : unit -> unit

module Terminated_by : sig
  type t =
    | End_of_line (** Line comments terminate at end of line *)
    | Comment_end (** Block comments terminate with a particular [comment-end] syntax. *)
  [@@deriving sexp_of]

  val in_current_buffer : unit -> t
end
