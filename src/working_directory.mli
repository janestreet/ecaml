open! Core
open! Import

type t =
  | Of_current_buffer
  | Root
  | This_abspath of File_path.Absolute.t
[@@deriving sexp_of]

val to_filename : t -> string

(** [within t ~f] runs [f] with [default-directory] bound to [t]. [f] must be synchronous:
    binding [default-directory] across an [await] would leak the temporary value to
    concurrently running Async jobs. *)
val within : t -> f:(unit -> 'r) -> 'r
