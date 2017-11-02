(** You can set up a timer to call a function at a specified future time or after a
    certain length of idleness.

    Emacs cannot run timers at any arbitrary point in a program; it can run them only when
    Emacs could accept output from a subprocess: namely, while waiting or inside certain
    primitive functions such as [sit-for] or [read-event] which _can_ wait.  Therefore, a
    timer's execution may be delayed if Emacs is busy.  However, the time of execution is
    very precise if Emacs is idle.

    [(Info-goto-node "(elisp)Timers")] *)

open! Core_kernel
open! Import

include Value.Subtype

(** [(describe-variable 'timer-list)] *)
val all_scheduled : unit -> t list

val is_scheduled : t -> bool

(** [(describe-function 'run-at-time)]
    [(Info-goto-node "(elisp)Timers")] *)
val run_after
  :  ?repeat : Time_ns.Span.t
  -> Time_ns.Span.t
  -> (unit -> unit)
  -> t

(** [run_after_i s f = ignore (run_after s f)] *)
val run_after_i
  :  ?repeat : Time_ns.Span.t
  -> Time_ns.Span.t
  -> (unit -> unit)
  -> unit

(** [(describe-function 'cancel-timer)]
    [(Info-goto-node "(elisp)Timers")] *)
val cancel : t -> unit

(** [(describe-function 'sit-for)]
    [(Info-goto-node "(elisp)Waiting")] *)
val sit_for
  :  ?redisplay : bool  (** default is [true] *)
  -> Time_ns.Span.t
  -> unit

(** [(describe-function 'sleep-for)]
    [(Info-goto-node "(elisp)Waiting")] *)
val sleep_for : Time_ns.Span.t -> unit
