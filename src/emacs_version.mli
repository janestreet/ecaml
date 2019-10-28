(** [Emacs_version] provides information about the version and build info of the running
    instance of Emacs.

    [(Info-goto-node "(elisp) Version Info")] *)

open! Core_kernel
open! Import

(** [(describe-function 'emacs-version)] *)
val version_string : unit -> string
