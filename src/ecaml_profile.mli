open! Core
open! Import

val print_length : int option Customization.t
val print_level : int option Customization.t

module Private : sig
  val tag_function : Function.t option Var.t
end
