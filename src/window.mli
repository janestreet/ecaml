(** A "window" in Emacs is the physical area of the screen in which a buffer is displayed.

    Each window has its own value of point, independent of the value of point in other
    windows displaying the same buffer.  This makes it useful to have multiple windows
    showing one buffer.

    A window is "live" as long as it displays a buffer.  All functions other than
    [is_live] raise if supplied a [t] such that [not (is_live t)].

    [(Info-goto-node "(elisp)Windows")]. *)

open! Core_kernel
open! Async_kernel
open! Import

type t = Window0.t [@@deriving sexp_of]

include Equal.S with type t := t
include Value.Subtype with type t := t

(** [(describe-function 'window-live-p)]. *)
val is_live : t -> bool

(** Accessors *)

(** [(describe-function 'window-body-height)] *)
val body_height_exn : t -> int

(** [(describe-function 'window-buffer)     ] *)
val buffer_exn : t -> Buffer.t

(** [(describe-function 'get-buffer-window)] *)
val get_buffer_window : Buffer.t -> t

(** [(describe-function 'window-height)     ] *)
val height_exn : t -> int

(** [(describe-function 'window-point)      ] *)
val point_exn : t -> Position.t

(** [(describe-function 'window-width)      ] *)
val width_exn : t -> int

(** [(describe-function 'set-window-buffer)] *)
val set_buffer_exn
  :  ?keep_margins:bool (** default is [false] *)
  -> t
  -> Buffer.t
  -> unit Deferred.t

(** [(describe-function 'set-window-point)] *)
val set_point_exn : t -> Position.t -> unit

(** [(Info-goto-node "(elisp)Display Margins")]
    [(describe-function 'set-window-margins)]*)
val set_window_margins : ?left_margin:int -> ?right_margin:int -> t -> unit

(** [(describe-function 'delete-window)] *)
val delete_exn : t -> unit

(** [(describe-function 'delete-other-windows)] *)
val delete_other_windows : t option -> unit

(** [(describe-function 'window-start)] *)
val start : t -> Position.t

(** [(describe-function 'set-window-start)] *)
val set_start : t -> Position.t -> unit

(** [(describe-function 'window-end)] *)
val end_ : ?update:bool (** default is [false] *) -> t -> Position.t

(** [(describe-function 'window-frame)] *)
val frame : t -> Frame.t

(** [(describe-function 'fit-window-to-buffer)] *)
val fit_to_buffer
  :  ?max_columns:int
  -> ?max_lines:int
  -> ?min_columns:int
  -> ?min_lines:int
  -> ?preserve_size:bool
  -> t
  -> unit

module Tree = Window0.Tree

module Blocking : sig
  val set_buffer_exn
    :  ?keep_margins:bool (** default is [false] *)
    -> t
    -> Buffer.t
    -> unit
end
