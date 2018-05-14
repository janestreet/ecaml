open! Core_kernel
open! Import0

module type Current_buffer0_public = sig
  (** [(describe-function 'current-buffer)] *)
  val get : unit -> Buffer0.t

  (** [(describe-function 'set-buffer)] *)
  val set : Buffer0.t -> unit

  val value        : 'a Var.t -> 'a option (** [(describe-function 'symbol-value)   ] *)
  val value_exn    : 'a Var.t -> 'a        (** [(describe-function 'symbol-value)   ] *)

  (** [(describe-function 'set)]. *)
  val set_value : 'a Var.t -> 'a -> unit

  (** [(describe-function 'makunbound)] *)
  val clear_value : 'a Var.t -> unit

  val set_value_temporarily : 'a Var.t -> 'a -> f:(unit -> 'b) -> 'b

  val set_values_temporarily : Var.And_value.t list -> f:(unit -> 'a) -> 'a

  (** [(describe-function 'bound-and-true-p]. *)
  val has_non_null_value : _ Var.t -> bool

end

module type Current_buffer0 = sig
  include Current_buffer0_public

  module Q : sig
    include module type of Q

    val boundp         : Symbol.t
    val current_buffer : Symbol.t
    val makunbound     : Symbol.t
    val set_buffer     : Symbol.t
  end
end
