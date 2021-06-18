(** [(Info-goto-node "(elisp)Backup Files")] *)

open! Core
open! Import

(** [(describe-variable 'make-backup-files)] *)
val make_backup_files : bool Customization.t
