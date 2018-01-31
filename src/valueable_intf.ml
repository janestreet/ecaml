open! Core_kernel
open! Import
open! Valueable0

module type S = S with type 'a type_ := 'a Value.Type.t

module type Valueable = sig
  module type S = S
end
