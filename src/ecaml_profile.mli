open! Core_kernel
open! Import
include Major_mode.S

val initialize : unit -> unit

module Private : sig
  val tag_function : Function.t option Var.t
end
