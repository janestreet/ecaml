(** A minibuffer is a special buffer that Emacs commands use to read arguments more
    complicated than the single numeric prefix argument.  These arguments include file
    names, buffer names, and command names (as in [M-x]).  The minibuffer is displayed on
    the bottom line of the frame, in the same place as the echo area, but only while it is
    in use for reading an argument.

    [(Info-goto-node "(elisp)Minibuffers")] *)

open! Core_kernel
open! Import
open! Async

module Y_or_n_with_timeout : sig
  type 'a t =
    | Y
    | N
    | Timeout of 'a
  [@@deriving sexp_of]
end

module History : sig
  type t [@@deriving sexp_of]

  val find_or_create : Symbol.t -> Source_code_position.t -> t
  val symbol : t -> Symbol.t
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

(** [(describe-function 'read-from-minibuffer)]
    [(Info-goto-node "(elisp)Text from Minibuffer")] *)
val read_from
  :  prompt:string
  -> ?initial_contents:string
  -> ?default_value:string
  -> history:History.t
  -> ?history_pos:int
  -> unit
  -> string Deferred.t

(** [(describe-function 'y-or-n-p)]
    [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val y_or_n : prompt:string -> bool Deferred.t

(** [(describe-function 'y-or-n-p-with-timeout)]
    [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val y_or_n_with_timeout
  :  prompt:string
  -> timeout:Time_ns.Span.t * 'a
  -> 'a Y_or_n_with_timeout.t Deferred.t

(** [(describe-function 'yes-or-no-p)]
    [(Info-goto-node "(elisp)Yes-or-No Queries")] *)
val yes_or_no : prompt:string -> bool Deferred.t

(** [(describe-variable 'minibuffer-exit-hook)]
    [(Info-goto-node "(elisp)Minibuffer Misc")] *)
val exit_hook : Hook.normal Hook.t

(** [(describe-variable 'minibuffer-setup-hook)]
    [(Info-goto-node "(elisp)Minibuffer Misc")] *)
val setup_hook : Hook.normal Hook.t

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
