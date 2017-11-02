(** [(Info-goto-node "(emacs)Comments")] *)

(** [(describe-variable 'comment-start)]
    [(Info-goto-node "(emacs)Options for Comments")] *)
val start : string Var.t

(** [(describe-variable 'comment-end)]
    [(Info-goto-node "(emacs)Options for Comments")] *)
val end_  : string Var.t

(** [(describe-variable 'comment-multi-line)]
    [(Info-goto-node "(emacs)Options for Comments")] *)
val multi_line : bool Var.t

val set_current_buffer_options
  :  start         : string
  -> end_          : string
  -> is_multi_line : bool
  -> unit
