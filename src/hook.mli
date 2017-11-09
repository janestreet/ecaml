(** A hook is a variable where you can store a function or functions to be called on a
    particular occasion by an existing program.  Emacs provides hooks for the sake of
    customization.

    Hooks installed through Ecaml are wrapped in an exception handler that prints the
    error to the [*Messages*] buffer and exits the function normally.

    [(Info-goto-node "(elisp)Hooks")] *)

open! Core_kernel
open! Import

type 'a t [@@deriving sexp_of]

type file = file : string -> unit
type normal = unit -> unit

module Type : sig
  type 'a t =
    | File   : file   t
    | Normal : normal t
  [@@deriving sexp_of]
end

val create : 'a Type.t -> Symbol.t -> 'a t

val var : _ t -> Function.t list Var.t

module Where : sig
  type t =
    | End
    | Start
  [@@deriving sexp_of]
end

module Function : sig
  type 'a t [@@deriving sexp_of]

  val create
    :  ?docstring : string
    -> Source_code_position.t
    -> 'a Type.t
    -> Symbol.t
    -> 'a
    -> 'a t
end

(** [(describe-function 'add-hook)]
    [(Info-goto-node "(elisp)Setting Hooks")] *)
val add
  :  ?buffer_local : bool (** default is [false] *)
  -> ?where : Where.t     (** default is [Start] *)
  -> 'a t
  -> 'a Function.t
  -> unit

(** [(describe-function 'remove-hook)]
    [(Info-goto-node "(elisp)Setting Hooks")] *)
val remove : 'a t -> 'a Function.t -> unit

val clear : _ t -> unit

(** [(describe-function 'run-hooks)]
    [(Info-goto-node "(elisp)Running Hooks")] *)
val run : normal t -> unit

(** [(describe-variable 'after-load-functions)]
    [(Info-goto-node "(elisp)Hooks for Loading")] *)
val after_load : file t

(** [after_load_once f] adds [f] to [after_load], and then removes it when it runs,
    so that [f] only runs once. *)
val after_load_once : file -> unit

(** [(describe-variable 'after-save-hook)]
    [(Info-goto-node "(elisp)Saving Buffers")] *)
val after_save : normal t

(** [(describe-variable 'kill-buffer-hook)]
    [(Info-goto-node "(elisp)Killing Buffers")] *)
val kill_buffer : normal t
