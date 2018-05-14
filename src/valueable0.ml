open! Core_kernel
open! Import0

module type S = sig
  type t
  type 'a type_

  val of_value_exn : Value0.t -> t
  val to_value     : t        -> Value0.t

  val type_ : t type_
end

module type S1 = sig
  type 'a t
  type 'a type_

  val of_value_exn : (Value0.t -> 'a) -> Value0.t -> 'a t
  val to_value     : ('a -> Value0.t) -> 'a t     -> Value0.t

  val type_ : 'a type_ -> 'a t type_
end
