open! Core_kernel
open! Import
include Major_mode.S

val print_length : int option Customization.t
val print_level : int option Customization.t

module Private : sig
  val tag_function : Function.t option Var.t
end
