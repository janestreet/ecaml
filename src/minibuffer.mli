(** A minibuffer is a special buffer that Emacs commands use to read arguments more
    complicated than the single numeric prefix argument. These arguments include file
    names, buffer names, and command names (as in [M-x]). The minibuffer is displayed on
    the bottom line of the frame, in the same place as the echo area, but only while it is
    in use for reading an argument.

    [(Info-goto-node "(elisp)Minibuffers")] *)

open! Core
open! Import
open! Async

module Y_or_n_with_timeout : sig
  type 'a t =
    | Y
    | N
    | Timeout of 'a
  [@@deriving sexp_of]
end

module Initial_input : sig
  type t =
    | Empty
    | Point_at_end of string
    (* 0-indexed position in the string. *)
    | Point_at_pos of string * int

  (* completing-read takes 0-indexed positions, read-from-minibuffer takes (weirdly)
     1-indexed positions, so they have different Elisp-side types so Initial_input.t is
     serialized differently. *)
  val completing_t : t Value.Type.t
  val minibuffer_t : t Value.Type.t
end

module History : sig
  type t [@@deriving sexp_of]

  val find_or_create : ?docstring:string -> here:[%call_pos] -> Symbol.t -> t
  val symbol : t -> Symbol.t

  (** [vc_revision] is `vc-revision-history', history for reading commit hashes. *)
  val vc_revision : t
end

(** [history] is the default history list used when reading from the minibuffer. *)
val history : History.t

module History_length : sig
  type t =
    | Truncate_after of int
    | No_truncation
  [@@deriving sexp_of]
end

(** [(describe-variable 'history-length)] *)
val history_length : History_length.t Customization.t

(** [(describe-function 'format-prompt)] *)
val format_prompt : prompt_no_colon:string -> default:string option -> string

(** [(describe-function 'read-string)] [(Info-goto-node "(elisp)Text from Minibuffer")] *)
val read_string
  :  prompt_no_colon:string
  -> ?initial_contents:Initial_input.t
  -> ?default_value:string
  -> history:History.t
  -> ?history_pos:int
  -> unit
  -> string Deferred.t

(** [(describe-function 'read-file-name)] [(Info-goto-node "(elisp) Reading File Names")] *)
val read_file_name
  :  prompt_no_colon:string
  -> ?directory:string
  -> ?default_filename:string
  -> ?mustmatch:string
  -> ?initial:string
  -> ?predicate:Function.t
  -> unit
  -> string Deferred.t

(** [(describe-function 'y-or-n-p)] [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val y_or_n : prompt:string -> bool Deferred.t

(** [(describe-function 'y-or-n-p-with-timeout)]
    [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val y_or_n_with_timeout
  :  prompt:string
  -> timeout:Time_ns.Span.t * 'a
  -> 'a Y_or_n_with_timeout.t Deferred.t

(** [(describe-function 'yes-or-no-p)] [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val yes_or_no : prompt:string -> bool Deferred.t

(** [(describe-variable 'minibuffer-exit-hook)]
    [(Info-goto-node "(elisp)Minibuffer Misc")] *)
val exit_hook : (Hook.normal, unit) Hook.t

(** [(describe-variable 'minibuffer-setup-hook)]
    [(Info-goto-node "(elisp)Minibuffer Misc")] *)
val setup_hook : (Hook.normal, unit) Hook.t
[@@alert
  prefer_with_setup_hook_instead
    "You probably want to use [with_setup_hook] to avoid re-entrancy bugs."]

(** - [(describe-function 'minibuffer-with-setup-hook)]
    - [(Info-goto-node "(elisp)Minibuffer Misc")] *)
val with_setup_hook
  :  ?append:bool
  -> here:[%call_pos]
  -> Function.t
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t

(** [(describe-function 'active-minibuffer-window)] *)
val active_window : unit -> Window.t option

(** [(describe-function 'minibuffer-prompt)] *)
val prompt : unit -> string option

(** [(describe-function 'exit-minibuffer)]

    This exits the minibuffer by throwing 'exit, so from OCaml's perspective it always
    raises. *)
val exit : unit -> never_returns

(** [(describe-function 'minibuffer-depth)] *)
val depth : unit -> int

(** [(describe-function 'minibuffer-contents)]

    Note: if the current buffer is not a minibuffer, [contents] returns its contents too. *)
val contents : unit -> string

(** [(describe-function 'minibuffer-message)] *)
val message : string -> unit
