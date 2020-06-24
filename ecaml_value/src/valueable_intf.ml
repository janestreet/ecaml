open! Core_kernel
open! Import
open! Valueable0

module type S = S with type 'a type_ := 'a Value.Type.t
module type S1 = S1 with type 'a type_ := 'a Value.Type.t

module Remove_t (M : S) : S with type t := M.t = M

module type Valueable = sig
  module type S = S
  module type S1 = S1

  module Remove_t = Remove_t

  val of_type : 'a Value.Type.t -> (module S with type t = 'a)
end
