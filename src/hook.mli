(** A hook is a variable where you can store a function or functions to be called on a
    particular occasion by an existing program.  Emacs provides hooks for the sake of
    customization.

    Hooks installed through Ecaml are wrapped in an exception handler that prints the
    error to the [*Messages*] buffer and exits the function normally.

    [(Info-goto-node "(elisp)Hooks")] *)

open! Core_kernel
open! Async_kernel
open! Import

type 'a t = 'a Hook0.t [@@deriving sexp_of]
type file = Hook0.file = { file : string } [@@deriving sexp_of]
type normal = unit [@@deriving sexp_of]

type window = Hook0.window =
  { window : Window.t
  ; start : Position.t
  }
[@@deriving sexp_of]

module Hook_type : sig
  type 'a t = 'a Hook0.Hook_type.t =
    | File : file t
    | Normal : normal t
    | Window : window t
  [@@deriving sexp_of]
end

val create : Symbol.t -> hook_type:'a Hook_type.t -> 'a t
val var : _ t -> Function.t list Var.t

module Where : sig
  type t =
    | End
    | Start
  [@@deriving sexp_of]
end

module Function : sig
  type 'a t [@@deriving sexp_of]

  (** [create here return_type symbol f] defines an emacs function named [symbol]
      that runs [f] when called. It returns an ['a t] usable for modifying hooks. *)
  val create
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> ?should_profile:bool
    -> hook_type:'a Hook_type.t
    -> (unit, 'r) Defun.Returns.t
    -> ('a -> 'r)
    -> 'a t

  (** [create_with_self here return_type symbol f] works the same as [create], except that
      the ['a t] it returns is also passed as an argument to [f].

      This is useful, for example, if [f] wants to remove itself from a hook once it is
      called. *)
  val create_with_self
    :  Symbol.t
    -> Source_code_position.t
    -> ?docstring:string
    -> hook_type:'a Hook_type.t
    -> (unit, 'r) Defun.Returns.t
    -> ('a t -> 'a -> 'r)
    -> 'a t
end

(** [(describe-function 'add-hook)]
    [(Info-goto-node "(elisp)Setting Hooks")]

    If [one_shot] is [true], the hook will be removed after the first time it runs. *)
val add
  :  ?buffer_local:bool (** default is [false] *)
  -> ?one_shot:bool (** default is [false] *)
  -> ?where:Where.t (** default is [Start] *)
  -> 'a t
  -> 'a Function.t
  -> unit

(** [(describe-function 'remove-hook)]
    [(Info-goto-node "(elisp)Setting Hooks")] *)
val remove : ?buffer_local:bool -> 'a t -> 'a Function.t -> unit

val clear : _ t -> unit

(** [(describe-function 'run-hooks)]
    [(Info-goto-node "(elisp)Running Hooks")] *)
val run : normal t -> unit Deferred.t

(** [(describe-variable 'after-load-functions)]
    [(Info-goto-node "(elisp)Hooks for Loading")] *)
val after_load : file t

(** [(describe-variable 'after-revert-hook)]
    [(Info-goto-node "(elisp)Reverting")] *)
val after_revert : normal t

(** [(describe-variable 'after-save-hook)]
    [(Info-goto-node "(elisp)Saving Buffers")] *)
val after_save : normal t

(** [(describe-variable 'before-save-hook)]
    [(Info-goto-node "(elisp)Saving Buffers")] *)
val before_save : normal t

(** [(describe-variable 'emacs-startup-hook)]

    Use this instead of [(describe-variable 'after-init-hook)], since the latter occurs
    before command-line arguments are handled (and therefore before the OCaml runtime is
    initialized). *)
val emacs_startup : normal t

(** [(describe-variable 'focus-in-hook)]
    [(Info-goto-node "(elisp)Input Focus")] *)
val focus_in : normal t

(** [(describe-variable 'kill-buffer-hook)]
    [(Info-goto-node "(elisp)Killing Buffers")] *)
val kill_buffer : normal t

(** [(describe-variable 'post-command-hook)]
    [(Info-goto-node "(elisp)Command Overview")] *)
val post_command : normal t

(** [(describe-variable 'window-configuration-change-hook)]
    [(Info-goto-node "(elisp)Window Hooks")] *)
val window_configuration_change : normal t

(** [(describe-variable 'window-scroll-functions)]
    [(Info-goto-node "(elisp)Window Hooks")] *)
val window_scroll_functions : window t

(** [(Info-goto-node "(elisp)Major Mode Conventions")] *)
val major_mode_hook : Major_mode.t -> normal t
