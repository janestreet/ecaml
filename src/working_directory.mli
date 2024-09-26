open! Core
open! Import

type t =
  | Of_current_buffer
  | Root
  | This of string [@deprecated "[since 2024-08] Use [This_abspath] instead."]
  | This_abspath of File_path.Absolute.t
[@@deriving sexp_of]

val to_filename : t -> string
val within : t -> (_, 'r) Sync_or_async.t -> f:(unit -> 'r) -> 'r
