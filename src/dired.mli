(** Dired, the Directory Editor

    [(Info-goto-node "(emacs) Dired")] *)

open! Core
open! Import

val major_mode : Major_mode.t

(** [(describe-function 'dired-get-marked-files)] *)
val get_marked_files : unit -> File_path.Absolute.t list
