open! Core_kernel
open! Import

module type Symbol = sig
  include Value.Subtype
  include Equal.S with type t := t
  include Value.Funcall with type t := t

  val intern : string -> t
  val name : t -> string
end
