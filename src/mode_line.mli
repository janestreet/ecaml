open! Core_kernel
open! Import

module Format : sig
  type t [@@deriving sexp_of]

  (** [(describe-variable 'mode-line-format)] *)
  val in_buffer : t Buffer_local.t
end

(** [(describe-function 'format-mode-line)]
    Sadly, in test, which runs Emacs batch mode, [text] always returns the empty string. *)
val text : Format.t -> Text.t
