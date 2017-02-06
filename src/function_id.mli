(** A unique id for each OCaml function that can be called from Emacs. *)

open! Core
open! Async
open! Import

include Unique_id.Id
