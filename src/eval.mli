open! Core_kernel
open! Import

(** [(describe-function 'eval-after-load)] *)
val after_load : Source_code_position.t -> Feature.t -> f:(unit -> unit) -> unit
