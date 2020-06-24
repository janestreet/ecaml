(** A "frame" is a screen object that contains one or more Emacs windows.  It is the kind
    of object called a "window" in the terminology of graphical environments; but we can't
    call it a "window" here, because Emacs uses that word in a different way.

    [(Info-goto-node "(elisp)Frames")]. *)

open! Core_kernel
open! Import0
include Value.Subtype

(** Accessors *)

(** [(describe-function 'make-frame)] *)
val create : ?name:string -> unit -> t

(** [(describe-function 'frame-width)] *)
val num_cols : t -> int

(** [(describe-function 'frame-height)] *)
val num_rows : t -> int

(** [(describe-function 'frame-pixel-height)] *)
val pixel_height : t -> int

(** [(describe-function 'frame-pixel-width)] *)
val pixel_width : t -> int

(** [(describe-function 'frame-parameters)] *)
val parameters : t -> (Symbol.t * Value.t) list

(** [(describe-function 'selected-frame)] *)
val selected : unit -> t

(** [(describe-function 'select-frame)] *)
val set_selected : t -> unit

(** [(describe-function 'with-selected-frame)] *)
val set_selected_temporarily : (_, 'a) Sync_or_async.t -> t -> f:(unit -> 'a) -> 'a

(** [(describe-function 'frame-visible-p)] *)
val is_visible : t -> bool

(** [(describe-function 'visible-frame-list)] *)
val all_visible : unit -> t list

(** [(describe-function 'frame-list)] *)
val all_live : unit -> t list

module Include_minibuffer : sig
  type t =
    | Yes
    | No
    | Only_if_active
  [@@deriving sexp_of]
end

(** [(describe-function 'window-list)] *)
val window_list : ?include_minibuffer:Include_minibuffer.t -> unit -> Window0.t list

(** [(describe-function 'frame-live-p)] *)
val is_live : t -> bool

(** [(describe-function 'other-frame)] *)
val other_frame : int -> unit

(** [(describe-function 'frame-terminal)] *)
val terminal : t -> Terminal.t

(** [(describe-variable 'frame-inherited-parameters)] *)
val inherited_parameters : Symbol.t list Var.t

(** [(describe-function 'window-tree)] *)
val window_tree : t -> Window0.Tree.t

(** [(describe-function 'modify-all-frames-parameters)] *)
val modify_all_frames_parameters : (Symbol.t * Value.t) list -> unit
