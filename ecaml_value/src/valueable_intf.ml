open! Core
open! Import
open! Valueable0

module type Type = sig
  type t

  val type_ : t Value.Type.t
end

module type S = S with type 'a type_ := 'a Value.Type.t
module type S1 = S1 with type 'a type_ := 'a Value.Type.t

module type Valueable = sig
  module type S = S
  module type S1 = S1

  module Make (M : Type) : S with type t := M.t
end
