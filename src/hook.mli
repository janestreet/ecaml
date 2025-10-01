(** A hook is a variable where you can store a function or functions to be called on a
    particular occasion by an existing program. Emacs provides hooks for the sake of
    customization.

    Hooks installed through Ecaml are wrapped in an exception handler that prints the
    error to the [*Messages*] buffer and exits the function normally.

    [(Info-goto-node "(elisp)Hooks")] *)

open! Core
open! Async_kernel
open! Import

type ('a, 'b) t = ('a, 'b) Hook0.t [@@deriving sexp_of]

type after_change = Hook0.after_change =
  { beginning_of_changed_region : Position.t
  ; end_of_changed_region : Position.t
  ; length_before_change : int
  }
[@@deriving sexp_of]

type before_change = Hook0.before_change =
  { beginning_of_changed_region : Position.t
  ; end_of_changed_region : Position.t
  }
[@@deriving sexp_of]

type file = Hook0.file = { file : string } [@@deriving sexp_of]
type normal = unit [@@deriving sexp_of]
type frame = Hook0.frame = { frame : Frame.t } [@@deriving sexp_of]

type window = Hook0.window =
  { window : Window.t
  ; start : Position.t
  }
[@@deriving sexp_of]

module Hook_type : sig
  type ('a, 'b) t = ('a, 'b) Hook0.Hook_type.t =
    | After_change_hook : (after_change, unit) t
    | Before_change_hook : (before_change, unit) t
    | File_hook : (file, unit) t
    | Normal_hook : (normal, unit) t
    | Frame_hook : (frame, unit) t
    | Window_hook : (window, unit) t
    | Query_function : (normal, bool) t
  [@@deriving sexp_of]
end

module Wrap : sig
  val ( <: ) : string -> ('a, 'b) Hook_type.t -> ('a, 'b) t
end

val var : _ t -> Function.t list Var.t

module Where : sig
  type t =
    | End
    | Start
  [@@deriving sexp_of]
end

module Function : sig
  type ('a, 'b) t [@@deriving sexp_of]

  (** [create here return_type symbol f] defines an emacs function named [symbol] that
      runs [f] when called. It returns an ['a t] usable for modifying hooks. *)
  val create
    :  Symbol.t
    -> Source_code_position.t
    -> docstring:string
    -> ?should_profile:bool
    -> ?demote_errors:bool (** default = [true] *)
    -> hook_type:('a, 'b) Hook_type.t
    -> ('b, 'r) Defun.Returns.t
    -> ('a -> 'r)
    -> ('a, 'b) t

  (** [create_with_self here return_type symbol f] works the same as [create], except that
      the ['a t] it returns is also passed as an argument to [f].

      This is useful, for example, if [f] wants to remove itself from a hook once it is
      called. *)
  val create_with_self
    :  Symbol.t
    -> Source_code_position.t
    -> docstring:string
    -> ?should_profile:bool
    -> ?demote_errors:bool (** default = [true] *)
    -> hook_type:('a, 'b) Hook_type.t
    -> ('b, 'r) Defun.Returns.t
    -> (('a, 'b) t -> 'a -> 'r)
    -> ('a, 'b) t

  (** [wrap symbol hook_type] wraps an existing function named [symbol] as a hook of
      [hook_type]. *)
  val wrap : Symbol.t -> hook_type:('a, 'b) Hook_type.t -> ('a, 'b) t

  val symbol : _ t -> Symbol.t
end

(** - [(describe-function 'add-hook)]
    - [(Info-goto-node "(elisp)Setting Hooks")] *)
val add
  :  here:[%call_pos]
  -> ?where:Where.t (** default is [Start] *)
  -> ('a, 'b) t
  -> ('a, 'b) Function.t
  -> unit

(** Like calling "add-hook" with LOCAL=t *)
val add_local
  :  ?where:Where.t (** default is [Start] *)
  -> ('a, 'b) t
  -> ('a, 'b) Function.t
  -> unit

(** - [(describe-function 'remove-hook)]
    - [(Info-goto-node "(elisp)Setting Hooks")] *)
val remove : ?buffer_local:bool -> ('a, 'b) t -> ('a, 'b) Function.t -> unit

val remove_symbol : ?buffer_local:bool -> ('a, 'b) t -> Symbol.t -> unit
val clear : _ t -> unit

(** - [(describe-function 'run-hooks)]
    - [(Info-goto-node "(elisp)Running Hooks")] *)
val run : (normal, unit) t -> unit Deferred.t

(** - [(describe-variable 'after-change-functions)]
    - [(Info-goto-node "(elisp)Change Hooks")] *)
val after_change_functions : (after_change, unit) t

(** - [(describe-variable 'after-load-functions)]
    - [(Info-goto-node "(elisp)Hooks for Loading")] *)
val after_load : (file, unit) t

(** - [(describe-variable 'after-revert-hook)]
    - [(Info-goto-node "(elisp)Reverting")] *)
val after_revert : (normal, unit) t

(** - [(describe-variable 'after-save-hook)]
    - [(Info-goto-node "(elisp)Saving Buffers")] *)
val after_save : (normal, unit) t

(** - [(describe-variable 'before-change-functions)]
    - [(Info-goto-node "(elisp)Change Hooks")] *)
val before_change_functions : (before_change, unit) t

(** - [(describe-variable 'before-save-hook)]
    - [(Info-goto-node "(elisp)Saving Buffers")] *)
val before_save : (normal, unit) t

(** - [(describe-variable 'change-major-mode-hook)]
    - [(Info-goto-node "(elisp) Creating Buffer-Local")] *)
val change_major_mode : (normal, unit) t

(** [(describe-variable 'emacs-startup-hook)]

    Use this instead of [(describe-variable 'after-init-hook)], since the latter occurs
    before command-line arguments are handled (and therefore before the OCaml runtime is
    initialized). *)
val emacs_startup : (normal, unit) t

(** - [(describe-variable 'focus-in-hook)]
    - [(Info-goto-node "(elisp)Input Focus")] *)
val focus_in : (normal, unit) t

(** - [(describe-variable 'kill-buffer-hook)]
    - [(Info-goto-node "(elisp)Killing Buffers")] *)
val kill_buffer : (normal, unit) t

(** - [(describe-variable 'kill-emacs-hook)]
    - [(Info-goto-node "(elisp) Killing Emacs")] *)
val kill_emacs : (normal, unit) t

(** - [(describe-variable 'post-command-hook)]
    - [(Info-goto-node "(elisp)Command Overview")] *)
val post_command : (normal, unit) t

(** - [(describe-variable 'pre-command-hook)]
    - [(Info-goto-node "(elisp)Command Overview")] *)
val pre_command : (normal, unit) t

(** - [(describe-variable 'window-configuration-change-hook)]
    - [(Info-goto-node "(elisp)Window Hooks")] *)
val window_configuration_change : (normal, unit) t

(** - [(describe-variable 'window-scroll-functions)]
    - [(Info-goto-node "(elisp)Window Hooks")] *)
val window_scroll_functions : (window, unit) t

(** [(Info-goto-node "(elisp)Major Mode Conventions")] *)
val major_mode_hook : Major_mode.t -> (normal, unit) t

(** [(describe-variable 'project-find-functions)] *)
val project_find_functions : (file, unit) t

(** - [(describe-variable 'write-contents-functions)]
    - [(Info-goto-node "(elisp) Saving Buffers")] *)
val write_contents_functions : (normal, bool) t
