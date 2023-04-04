(** [Emacs_version] provides information about the version and build info of the running
    instance of Emacs.

    [(Info-goto-node "(elisp) Version Info")] *)

open! Core
open! Import

(** [(describe-function 'emacs-version)] *)
val version_string : unit -> string

(** [(describe-variable 'emacs-major-version)] *)
val major_version : int
