open! Core_kernel
open! Import

module type S = sig
  type t
  type 'a type_

  val of_value_exn : Value0.t -> t
  val to_value : t -> Value0.t

  val type_ : t type_
end
