open! Core
open! Import

type t =
  | Of_current_buffer
  | Root
  | This_abspath of File_path.Absolute.t
[@@deriving sexp_of]

val to_filename : t -> string
val within : t -> (_, 'r) Sync_or_async.t -> f:(unit -> 'r) -> 'r
