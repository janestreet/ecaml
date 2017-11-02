(** A [Marker.t] specifies a position in a buffer relative to the surrounding text.  A
    marker changes its offset from the beginning of the buffer automatically whenever text
    is inserted or deleted, so that it stays with the two characters on either side of it.

    [(Info-goto-node "(elisp)Markers")] *)

open! Core_kernel
open! Import

include Value.Subtype

(** When you insert text directly at the place where a marker points, there are two
    possible ways to relocate that marker: it can point before the inserted text, or point
    after it.  You can specify which one a given marker should do by setting its
    "insertion type".  [(Info-goto-node "(elisp)Marker Insertion Types")]. *)
module Insertion_type : sig
  type t =
    | After_inserted_text
    | Before_inserted_text
  [@@deriving sexp_of]
end

(** [(describe-function 'marker-buffer)]
    [(Info-goto-node "(elisp)Information from Markers")] *)
val buffer : t -> Buffer.t option

(** [(describe-function 'marker-insertion-type)]
    [(Info-goto-node "(elisp)Marker Insertion Types")] *)
val insertion_type : t -> Insertion_type.t

(** [(describe-function 'marker-position)]
    [(Info-goto-node "(elisp)Information from Markers")] *)
val position : t -> Position.t option

(** [(describe-function 'make-marker)]
    [(Info-goto-node "(elisp)Creating Markers")] *)
val create : unit -> t

(** [(describe-function 'copy-marker)]
    [(Info-goto-node "(elisp)Creating Markers")] *)
val copy : t -> t

(** [(describe-function 'set-marker-insertion-type)]
    [(Info-goto-node "(elisp)Marker Insertion Types")] *)
val set_insertion_type : t -> Insertion_type.t -> unit

(** [(describe-function 'set-marker)]
    [(Info-goto-node "(elisp)Moving Markers")] *)
val set : t -> Buffer.t -> Position.t -> unit
