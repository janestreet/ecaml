(** This module exists to break the dependency between [Value] and [Callback]. *)

open! Core
open! Async
open! Import

type t
