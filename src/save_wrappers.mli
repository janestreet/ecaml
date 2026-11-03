open! Core
open! Import

(** [(describe-function 'save-excursion)] *)
val save_excursion : here:[%call_pos] -> (unit -> 'a) -> 'a

(** [(describe-function 'save-match-data)] *)
val save_match_data : here:[%call_pos] -> (unit -> 'a) -> 'a

(** [(describe-function 'save-restriction)] *)
val save_restriction : here:[%call_pos] -> (unit -> 'a) -> 'a

(** [(describe-function 'save-window-excursion)] *)
val save_window_excursion : here:[%call_pos] -> (unit -> 'a) -> 'a

(** [(describe-function 'save-selected-window)] *)
val save_selected_window : here:[%call_pos] -> (unit -> 'a) -> 'a

(** [(describe-function 'with-selected-frame)] *)
val with_selected_frame : here:[%call_pos] -> Value.t -> (unit -> 'a) -> 'a
