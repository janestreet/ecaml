(** [(Info-goto-node "(elisp)Display")] *)

open! Core
open! Import

(** [(describe-function 'redisplay)] [(Info-goto-node "(elisp)Forcing Redisplay")] *)
val redisplay : ?force:bool (** default is [false] *) -> unit -> unit

(** [(describe-variable 'inhibit-redisplay)] *)
val inhibit_redisplay : bool Var.t

(** [(describe-function 'display-monitor-attributes-list)] *)
val monitor_attributes : unit -> (Symbol.t * Value.t) list list
