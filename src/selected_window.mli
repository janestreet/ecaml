(** The selected window is the one that you edit in. When a window is selected, the buffer
    in the window becomes the current buffer, and the cursor will appear in it.

    [(Info-goto-node "(elisp)Selecting Windows")]. *)

open! Core
open! Async_kernel
open! Import

(** [(describe-function 'selected-window)] *)
val get : unit -> Window.t

(** [(describe-function 'select-window)] *)
val set : ?move_to_front_of_buffer_list:bool -> Window.t -> unit

(** [(describe-function 'pop-to-buffer-same-window)] *)
val pop_to_buffer : Buffer.t -> unit Deferred.t

(** [(describe-function 'switch-to-buffer-other-window)] *)
val switch_to_buffer_other_window : Buffer.t -> unit Deferred.t

(** [(describe-function 'split-window-horizontally)] *)
val split_horizontally_exn : unit -> unit

(** [(describe-function 'split-window-sensibly)] *)
val split_sensibly_exn : unit -> unit

(** [(describe-function 'split-window-vertically)] *)
val split_vertically_exn : unit -> unit

(** [(describe-function 'find-file)] *)
val find_file : string -> unit Deferred.t

(** [(describe-function 'find-file-other-window)] *)
val find_file_other_window : string -> unit Deferred.t

(** [(describe-function 'find-file-read-only)] *)
val find_file_read_only : string -> unit Deferred.t

(** [(describe-function 'save-selected-window)] *)
val save_selected_window : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'with-selected-window)]. Avoid using [set_temporarily], which can
    have visual artifacts that annoy the user, like flicker of the mode line or cursor. *)
val set_temporarily : (_, 'a) Sync_or_async.t -> Window.t -> f:(unit -> 'a) -> 'a

(** [(describe-function 'save-window-excursion)] *)
val save_window_excursion : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'quit-window)] *)
val quit : unit -> unit Deferred.t

(** [(describe-function 'other-window)] *)
val other_window : int -> unit

(** [(describe-function 'window-height)] *)
val height : unit -> int

(** [(describe-function 'window-width)] *)
val width : unit -> int

(** Return the number of screen lines in the selected window before the screen line where
    point is displayed.

    The return value can be passed to {!recenter} to restore the approximate vertical
    position of point from when this function was called. *)
val screen_line_at_point : unit -> int

module Blocking : sig
  val pop_to_buffer : Buffer.t -> unit
end
