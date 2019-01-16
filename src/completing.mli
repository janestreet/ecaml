(** Prompts for input, with completion from a static collection of suggestions. *)

open! Core_kernel
open! Import
open! Async

module Initial_input : sig
  type t =
    | Empty
    | Point_at_end of string
    | Point_at_pos of string * int

  include Valueable.S with type t := t
end

module Require_match : sig
  (** Must input match a completion candidate? *)
  type t =
    | Confirm  (** Can exit without match by confirming *)
    | Confirm_after_completion
    (** Can exit without match by typing, or by completing and then confirming *)
    | False  (** Can exit without match *)
    | Require_match_or_null  (** Cannot exit without match unless the input is nil *)
    | True  (** Cannot exit without match *)

  include Valueable.S with type t := t

  (** False *)
  val default : t
end

(** [(describe-function 'completing-read)] **)
val read
  :  ?default:string
  -> ?history:Symbol.t
  -> ?initial_input:Initial_input.t (* default is Empty *)
  -> ?require_match:Require_match.t (* default is [Require_match.default] *)
  -> unit
  -> collection:string list
  -> prompt:string (* typically ends with ": " *)
  -> string Deferred.t

(** [(describe-function 'completing-read-multiple)] **)
val read_multiple
  :  ?default:string
  -> ?history:Symbol.t
  -> ?initial_input:Initial_input.t (* default is Empty *)
  -> ?require_match:Require_match.t (* default is False *)
  -> ?separator_regexp:string (* default is "[ \t]*,[ \t]*" *)
  -> unit
  -> collection:string list
  -> prompt:string (* typically ends with ": " *)
  -> string list Deferred.t

module Blocking : sig
  (** [(describe-function 'completing-read)] **)
  val read
    :  ?default:string
    -> ?history:Symbol.t
    -> ?initial_input:Initial_input.t (* default is Empty *)
    -> ?require_match:Require_match.t (* default is [Require_match.default] *)
    -> unit
    -> collection:string list
    -> prompt:string (* typically ends with ": " *)
    -> string
end
