open! Core
open! Import

(** [(describe-function 'eval-after-load)] *)
val after_load : ?here:Stdlib.Lexing.position -> Feature.t -> f:(unit -> unit) -> unit
