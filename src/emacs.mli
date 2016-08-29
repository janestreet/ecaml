(** This module contains functions for low-level interaction with the Emacs user
    interface.

    Typically, functions in this module call the eponymous Elisp functions. *)

open! Core.Std
open! Async.Std
open! Import

val message_t : Value.t -> unit
val message   : string  -> unit
val messagef : ('a, unit, string, unit) format4 -> 'a
