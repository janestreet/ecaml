open! Core_kernel
open! Import

type t =
  { col : int
  ; row : int
  }
[@@deriving sexp_of]

(** [of_position position] converts [position] to the corresponding column and row in
    the current buffer. *)
val of_position : Position.t -> t

(** [to_position position] converts [t] to the corresponding position in the current
    buffer. *)
val to_position : t -> Position.t

(** Move the point to the specified column and row in the current buffer. *)
val goto : t -> unit
