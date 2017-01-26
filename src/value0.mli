(** This module exists to break the dependency between [Value] and [Callback]. *)

open! Core
open! Async.Std
open! Import

type t
