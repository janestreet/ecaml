(** This module exists to break the dependency between [Value] and [Callback]. *)

open! Core.Std
open! Async.Std
open! Import

type t
