(** A unique id for each OCaml function that can be called from Emacs. *)

open! Core.Std
open! Async.Std
open! Import

include Unique_id.Id
