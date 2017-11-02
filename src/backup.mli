(** [(Info-goto-node "(elisp)Backup Files")] *)

open! Core_kernel
open! Import

(** [(describe-variable 'make-backup-files)] *)
val make_backup_files : bool Var.t
