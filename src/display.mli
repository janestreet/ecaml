(** [(Info-goto-node "(elisp)Display")] *)

open! Core_kernel
open! Import

(** [(describe-function 'redisplay)]
    [(Info-goto-node "(elisp)Forcing Redisplay")] *)
val redisplay
  :  ?force : bool (** default is [false] *)
  -> unit
  -> unit

(** [(describe-variable 'inhibit-redisplay)] *)
val inhibit_redisplay : bool Var.t
