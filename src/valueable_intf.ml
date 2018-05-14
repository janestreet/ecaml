open! Core_kernel
open! Import0
open! Valueable0

module type S  = S  with type 'a type_ := 'a Value.Type.t
module type S1 = S1 with type 'a type_ := 'a Value.Type.t

module type Valueable = sig
  module type S  = S
  module type S1 = S1
end
