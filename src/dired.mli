(** Dired, the Directory Editor

    [(Info-goto-node "(emacs) Dired")] *)

open! Core
open! Import
include Major_mode.S

val get_marked_files : unit -> File_path.Absolute.t list
