(** A "frame" is a screen object that contains one or more Emacs windows.  It is the kind
    of object called a "window" in the terminology of graphical environments; but we can't
    call it a "window" here, because Emacs uses that word in a different way.

    [(Info-goto-node "(elisp)Frames")]. *)

open! Core_kernel
open! Import0
include Value.Subtype

(** Accessors *)

(** [(describe-function 'make-frame)] *)
val create : unit -> t

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
val set_selected_temporarily : t -> f:(unit -> 'a) -> 'a

(** [(describe-function 'visible-frame-list)] *)
val all_visible : unit -> t list

(** [(describe-function 'frame-list)] *)
val all_live : unit -> t list

(** [(describe-function 'frame-live-p)] *)
val is_live : t -> bool

(** [(describe-function 'other-frame)] *)
val other_frame : int -> unit

(** [(describe-function 'frame-terminal)] *)
val terminal : t -> Terminal.t
