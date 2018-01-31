open! Core_kernel
open! Import

(* Prompts for input, with completion from a static collection of suggestions. *)

module Initial_input : sig
  type t =
    | Empty
    | Point_at_end of string
    | Point_at_pos of string * int
end

module Require_match : sig
  (** Must input match a completion candidate? *)
  type t =
    (** Can exit without match by confirming *)
    | Confirm
    (** Can exit without match by typing, or by completing and then confirming *)
    | Confirm_after_completion
    (** Can exit without match *)
    | False
    (** Cannot exit without match unless the input is nil *)
    | Require_match_or_null
    (** Cannot exit without match *)
    | True
end

(** [(describe-function 'completing-read)] **)
val read
  :  ?default : string
  -> ?history : Symbol.t
  -> ?initial_input : Initial_input.t  (* default is Empty *)
  -> ?require_match : Require_match.t  (* default is False *)
  -> unit
  -> collection : string list
  -> prompt : string  (* typically ends with ": " *)
  -> string

(** [(describe-function 'completing-read-multiple)] **)
val read_multiple
  :  ?default : string
  -> ?history : Symbol.t
  -> ?initial_input : Initial_input.t  (* default is Empty *)
  -> ?require_match : Require_match.t  (* default is False *)
  -> ?separator_regexp : string        (* default is "[ \t]*,[ \t]*" *)
  -> unit
  -> collection : string list
  -> prompt : string  (* typically ends with ": " *)
  -> string list
