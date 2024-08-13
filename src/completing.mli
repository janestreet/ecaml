(** Prompts for input, with completion from a static collection of suggestions. *)

open! Core
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
    | Confirm (** Can exit without match by confirming *)
    | Confirm_after_completion
    (** Can exit without match by typing, or by completing and then confirming *)
    | False (** Can exit without match *)
    | Require_match_or_null__confirm_if_ret_completes
    (** Cannot exit without match unless the input is empty.  If the user presses RET and
        the input completes to a matching candidate, pause for confirmation before
        accepting the input. *)
    | Require_match_or_complete_or_null
    (** Cannot exit without match unless the input is empty.  If the user presses RET and
        the input completes to a matching candidate, accept the completed candidate. *)

  include Valueable.S with type t := t

  (** False *)
  val default : t
end

(** [(describe-function 'completing-read)] **)
val read
  :  prompt:string (** typically ends with ": " *)
  -> collection:string list
  -> ?annotation_function:(string -> string)
  -> ?display_sort_function:(string list -> string list)
  -> ?require_match:Require_match.t (** default is [Require_match.default] *)
  -> ?initial_input:Initial_input.t (** default is Empty *)
  -> ?default:string
  -> history:Minibuffer.History.t
  -> unit
  -> string Deferred.t

(** If [confirm_ret_completion = false], behaves like [~require_match:True].

    If [confirm_ret_completion = true], behaves like
    [~require_match:Require_match_or_null__confirm_if_ret_completes]. *)
val read_map_key
  :  prompt:string (** typically ends with ": " *)
  -> collection:'a String.Map.t
  -> ?annotation_function:(string -> string)
  -> ?display_sort_function:(string list -> string list)
  -> ?initial_input:Initial_input.t (** default is Empty *)
  -> ?default:string
  -> ?confirm_ret_completion:bool (** default is false *)
  -> history:Minibuffer.History.t
  -> unit
  -> 'a Deferred.t

(** Read a function name in the same manner as [describe-function]. *)
val read_function_name
  :  prompt:string
  -> history:Minibuffer.History.t
  -> string Deferred.t

(** Read a variable name in the same manner as [describe-variable]. *)
val read_variable_name
  :  prompt:string
  -> history:Minibuffer.History.t
  -> string Deferred.t

(** [(describe-function 'completing-read-multiple)] **)
val read_multiple
  :  prompt:string (** typically ends with ": " *)
  -> collection:string list
  -> ?require_match:Require_match.t (** default is False *)
  -> ?separator_regexp:string (** default is "[ \t]*,[ \t]*" *)
  -> ?initial_input:Initial_input.t (** default is Empty *)
  -> ?default:string
  -> history:Minibuffer.History.t
  -> unit
  -> string list Deferred.t

val read_multiple_map_keys
  :  prompt:string (** typically ends with ": " *)
  -> collection:'a String.Map.t
  -> ?separator_regexp:string (** default is "[ \t]*,[ \t]*" *)
  -> ?initial_input:Initial_input.t (** default is Empty *)
  -> ?default:string
  -> history:Minibuffer.History.t
  -> unit
  -> 'a list Deferred.t
