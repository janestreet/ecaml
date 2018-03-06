(** The selected window is the one that you edit in.  When a window is selected, the
    buffer in the window becomes the current buffer, and the cursor will appear in it.

    [(Info-goto-node "(elisp)Selecting Windows")]. *)

open! Core_kernel
open! Import

(** [(describe-function 'selected-window)] *)
val get : unit -> Window.t

(** [(describe-function 'select-window)] *)
val set
  :  ?move_to_front_of_buffer_list : bool
  -> Window.t
  -> unit

(** [(describe-function 'switch-to-buffer)] *)
val switch_to_buffer : Buffer.t -> unit

(** [(describe-function 'split-window-horizontally)] *)
val split_horizontally_exn : unit -> unit

(** [(describe-function 'split-window-vertically)] *)
val split_vertically_exn : unit -> unit

(** [(describe-function 'find-file)] *)
val find_file : string -> unit

(** [(describe-function 'view-file)] *)
val view_file : string -> unit

(** [(describe-function 'save-selected-window)] *)
val save_selected_window : (unit -> 'a) -> 'a

(** [(describe-function 'save-window-excursion)] *)
val save_window_excursion : (unit -> 'a) -> 'a
