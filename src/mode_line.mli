open! Core
open! Import

module Format : sig
  type t [@@deriving sexp_of]

  (** [(describe-variable 'mode-line-format)] *)
  val in_buffer : t Buffer_local.t

  (** [(describe-variable 'header-line-format)] *)
  val header_line_in_buffer : t Buffer_local.t

  val empty : t

  (** Quotes any %-constructs in the string, so that the string is interpreted verbatim in
      the mode line format. *)
  val string_verbatim : string -> t

  val propertize : t -> Text.Property.t list -> t
end

(** [(describe-function 'format-mode-line)] Sadly, in test, which runs Emacs batch mode,
    [text] always returns the empty string. *)
val text : Format.t -> Text.t
