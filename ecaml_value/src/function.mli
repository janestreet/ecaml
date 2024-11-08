(** Lower-level interface for functions that call from Emacs to OCaml.

    Try [Defun] first. *)

open! Core
open! Import
open! Async_kernel

module Fn : sig
  type t = Value.t array -> Value.t
end

include Value.Subtype

module Interactive : sig
  type t =
    | Args of (unit -> Value.t list Deferred.t)
    (** When a command defined with [~interactive:(Args f)] is called interactively, [f
        ()] is called to compute the argument values to supply to the command.  Of
        course, the argument values should match the command's [Defun.t]
        specification. *)
    | Form of Form.t
    | Function_name of { prompt : string }
    | Ignored
    | No_arg
    | Prompt of string
    | Raw_prefix
    | Prefix
    | Region

  (** An interactive form which evaluates to a list of constant values. *)
  val list : Value.t list -> t
end

val create
  :  Source_code_position.t
  -> ?docstring:string
  -> ?interactive:Interactive.t
  -> args:Symbol.t list
  -> ?optional_args:Symbol.t list
  -> ?rest_arg:Symbol.t
  -> Fn.t
  -> t

val create_nullary
  :  Source_code_position.t
  -> ?docstring:string
  -> ?interactive:Interactive.t
  -> (unit -> unit)
  -> t

val to_value : t -> Value.t
val of_symbol : Symbol.t -> t

module Expert : sig
  val raise_in_dispatch : bool ref
end
