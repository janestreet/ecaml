(** [(Info-goto-node "(elisp)Command Loop")] *)

open! Core_kernel
open! Async_kernel
open! Import0

(** A [Command.t] is an Elisp value satsifying [commandp].
    [(describe-function 'commandp)] *)
include
  Value.Subtype

(** [(Info-goto-node "(elisp)Prefix Command Arguments")] *)
module Raw_prefix_argument : sig
  type t =
    | Absent
    | Int of int
    | Minus
    | Nested of int
  [@@deriving sexp_of]

  val of_value_exn : Value.t -> t
  val to_value : t -> Value.t

  (** [(describe-variable 'current-prefix-arg)]
      [(Info-goto-node "(elisp)Prefix Command Arguments")] *)
  val for_current_command : t Var.t

  (** [(describe-function 'prefix-numeric-value)]
      [(Info-goto-node "(elisp)Prefix Command Arguments")] *)
  val numeric_value : t -> int
end

(** [(describe-variable 'command-history)]
    [(Info-goto-node "(elisp)Command History")] *)
val history : unit -> Form.t list

(** [(describe-function 'call-interactively)]
    [(Info-goto-node "(elisp)Interactive Call")] *)
val call_interactively
  :  ?raw_prefix_argument:Raw_prefix_argument.t (** default is [Absent] *)
  -> ?record:bool (** default is [false] *)
  -> Value.t
  -> unit Deferred.t

(** [(describe-variable 'inhibit-quit)]
    [(Info-goto-node "(elisp)Quitting")] *)
val inhibit_quit : bool Var.t

(** [(describe-variable 'quit-flag)]
    [(Info-goto-node "(elisp)Quitting")] *)
val quit_flag : bool Var.t

val quit_requested : unit -> bool
val request_quit : unit -> unit

(** [(describe-function 'abort-recursive-edit)] *)
val abort_recursive_edit : unit -> never_returns
