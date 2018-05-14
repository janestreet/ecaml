(** A "frame" is a screen object that contains one or more Emacs windows.  It is the kind
    of object called a "window" in the terminology of graphical environments; but we can't
    call it a "window" here, because Emacs uses that word in a different way.

    [(Info-goto-node "(elisp)Frames")]. *)

open! Core_kernel
open! Import0

include Value.Subtype

(** Accessors *)
val num_cols     : t -> int (** [(describe-function 'frame-width)       ] *)
val num_rows     : t -> int (** [(describe-function 'frame-height)      ] *)
val pixel_height : t -> int (** [(describe-function 'frame-pixel-height)] *)
val pixel_width  : t -> int (** [(describe-function 'frame-pixel-width) ] *)

(** [(describe-function 'frame-parameters)] *)
val parameters : t -> (Symbol.t * Value.t) list

(** [(describe-function 'selected-frame)] *)
val selected : unit -> t

(** [(describe-function 'select-frame)] *)
val set_selected : t -> unit

(** [(describe-function 'visible-frame-list)] *)
val all_visible : unit -> t list

(** [(describe-function 'frame-list)] *)
val all_live : unit -> t list
