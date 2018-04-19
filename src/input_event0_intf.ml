open! Core_kernel
open! Import

module type Input_event0_public = sig
  include Value.Subtype

  (** [(describe-function 'single-key-description)]
      [(Info-goto-node "(elisp)Describing Characters")] *)
  val description : t -> string
end

module type Input_event0 = sig
  include Input_event0_public

  module Q : sig
    include module type of Q
    val single_key_description : Symbol.t
  end
end
