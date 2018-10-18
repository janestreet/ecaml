(** Overlays modify the appearance of a buffer's text, for presentation's sake.

    [(Info-goto-node "(elisp)Overlays")] *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

include Valueable.S with type t := t

(** [(describe-function 'make-overlay)] *)
val create : ?buffer:Buffer.t -> unit -> start:Position.t -> end_:Position.t -> t

(** [(describe-function 'overlay-buffer)] *)
val buffer : t -> Buffer.t

(** [(describe-function 'overlay-start)] *)
val start : t -> Position.t

(** [(describe-function 'overlay-end)] *)
val end_ : t -> Position.t

(** [(describe-function 'delete-overlay)] *)
val delete : t -> unit

(** [(describe-function 'move-overlay)] *)
val move : ?buffer:Buffer.t -> t -> start:Position.t -> end_:Position.t -> t

(** [(describe-function 'overlay-get)] *)
val get_property : t -> 'a Text.Property_name.t -> 'a

(** [(describe-function 'overlay-put)] *)
val put_property : t -> 'a Text.Property_name.t -> 'a -> unit

(** [(describe-function 'overlays-at)] *)
val at : Position.t -> t list

(** [(describe-function 'remove-overlays)].
    [(Info-goto-node "(elisp)Managing Overlays")] *)
val remove_overlays
  :  ?start:Position.t
  -> ?end_:Position.t
  -> ?with_property:'a Text.Property_name.t * 'a
  -> unit
  -> unit

(** [(describe-function 'overlays-in)] *)
val in_ : start:Position.t -> end_:Position.t -> t list

val equal : t -> t -> bool
