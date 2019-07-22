open! Core_kernel
open! Import

(** [(describe-function 'save-current-buffer)] *)
val save_current_buffer : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-excursion)] *)
val save_excursion : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-mark-and-excursion)] *)
val save_mark_and_excursion : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-match-data)] *)
val save_match_data : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-restriction)] *)
val save_restriction : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-window-excursion)] *)
val save_window_excursion : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-selected-window)] *)
val save_selected_window : (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'with-selected-frame)] *)
val with_selected_frame : (_, 'a) Sync_or_async.t -> Value.t -> (unit -> 'a) -> 'a

(** [(describe-function 'with-selected-window)] *)
val with_selected_window : (_, 'a) Sync_or_async.t -> Value.t -> (unit -> 'a) -> 'a
