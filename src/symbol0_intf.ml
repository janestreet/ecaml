open! Core_kernel
open! Import0

module type Symbol0 = sig
  include Value.Subtype
  include Equal.S with type t := t
  include Value.Funcall with type t := t

  val intern : string -> t
end
