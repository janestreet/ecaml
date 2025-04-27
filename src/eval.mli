open! Core
open! Import

(** [(describe-function 'eval-after-load)] *)
val after_load : here:[%call_pos] -> Feature.t -> f:(unit -> unit) -> unit
