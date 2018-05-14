(** A color name specifies a color.

    [(Info-goto-node "(elisp)Color Names")]. *)

open! Core_kernel
open! Import0

include Value.Subtype

val of_name : string -> t
val to_name : t -> string

val black   : t
val blue    : t
val cyan    : t
val green   : t
val magenta : t
val orange  : t
val red     : t
val white   : t
val yellow  : t

(** [(describe-function 'color-gray-p)] *)
val is_gray
  :  ?on:Frame.t  (** default is selected frame *)
  -> t
  -> bool

(** [(describe-function 'color-defined-p)] *)
val is_defined
  :  ?on:Frame.t  (** default is selected frame *)
  -> t
  -> bool

(** [(describe-function 'color-supported-p)] *)
val is_supported
  :  ?on:Frame.t  (** default is selected frame *)
  -> t
  -> bool

(** [(describe-function 'defined-colors)] *)
val defined : ?on:Frame.t -> unit -> t list

module RGB : sig
  (** We've observed that [r], [g], and [b] are in the range [0, 65_535]. *)
  type t =
    { r : int
    ; g : int
    ; b : int }
  [@@deriving sexp_of]

  val max_value : int
  val min_value : int

  val map : t -> f:(int -> int) -> t
end

(** [(describe-function 'color-values)] *)
val rgb_exn
  :  ?on:Frame.t  (** default is selected frame *)
  -> t
  -> RGB.t

val of_rgb : RGB.t -> t
val of_rgb8 : r:int -> g:int -> b:int ->t
