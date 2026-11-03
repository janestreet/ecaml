open! Core
open! Import

(** [(describe-function 'save-excursion)] *)
val save_excursion : ?here:Stdlib.Lexing.position -> (unit -> 'a) -> 'a

(** [(describe-function 'save-match-data)] *)
val save_match_data : ?here:Stdlib.Lexing.position -> (unit -> 'a) -> 'a

(** [(describe-function 'save-restriction)] *)
val save_restriction : ?here:Stdlib.Lexing.position -> (unit -> 'a) -> 'a

(** [(describe-function 'save-window-excursion)] *)
val save_window_excursion : ?here:Stdlib.Lexing.position -> (unit -> 'a) -> 'a

(** [(describe-function 'save-selected-window)] *)
val save_selected_window : ?here:Stdlib.Lexing.position -> (unit -> 'a) -> 'a

(** [(describe-function 'with-selected-frame)] *)
val with_selected_frame : ?here:Stdlib.Lexing.position -> Value.t -> (unit -> 'a) -> 'a
