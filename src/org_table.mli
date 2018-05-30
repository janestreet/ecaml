(** [(Info-goto-node "(org)Orgtbl mode")] *)

open! Import

(** [org-table] *)
val feature : Symbol.t

(** [(describe-variable 'orgtbl-mode)]
    [(describe-function 'orgtbl-mode)]*)
val minor_mode : Minor_mode.t

(** [(describe-function 'turn-on-orgtbl)]*)
val enable_minor_mode : unit -> unit
