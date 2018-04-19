open! Core_kernel
open! Import

(** [(describe-function 'save-current-buffer)] *)
val save_current_buffer : (unit -> 'a) -> 'a

(** [(describe-function 'save-excursion)] *)
val save_excursion : (unit -> 'a) -> 'a

(** [(describe-function 'save-mark-and-excursion)] *)
val save_mark_and_excursion : (unit -> 'a) -> 'a

(** [(describe-function 'save-restriction)] *)
val save_restriction : (unit -> 'a) -> 'a

(** [(describe-function 'save-window-excursion)] *)
val save_window_excursion : (unit -> 'a) -> 'a

(** [(describe-function 'save-selected-window)] *)
val save_selected_window : (unit -> 'a) -> 'a
