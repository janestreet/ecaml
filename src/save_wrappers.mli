open! Core
open! Import

(** [(describe-function 'save-current-buffer)] *)
val save_current_buffer
  :  here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> (unit -> 'a)
  -> 'a

(** [(describe-function 'save-excursion)] *)
val save_excursion : here:[%call_pos] -> (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-mark-and-excursion)] *)
val save_mark_and_excursion
  :  here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> (unit -> 'a)
  -> 'a

(** [(describe-function 'save-match-data)] *)
val save_match_data : here:[%call_pos] -> (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-restriction)] *)
val save_restriction : here:[%call_pos] -> (_, 'a) Sync_or_async.t -> (unit -> 'a) -> 'a

(** [(describe-function 'save-window-excursion)] *)
val save_window_excursion
  :  here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> (unit -> 'a)
  -> 'a

(** [(describe-function 'save-selected-window)] *)
val save_selected_window
  :  here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> (unit -> 'a)
  -> 'a

(** [(describe-function 'with-selected-frame)] *)
val with_selected_frame
  :  here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> Value.t
  -> (unit -> 'a)
  -> 'a

(** [(describe-function 'with-selected-window)] *)
val with_selected_window
  :  here:[%call_pos]
  -> (_, 'a) Sync_or_async.t
  -> Value.t
  -> (unit -> 'a)
  -> 'a
