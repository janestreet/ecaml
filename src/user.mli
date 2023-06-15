open! Core
open! Import

(** [(describe-function 'user-login-name)]
    [(Info-goto-node "(elisp) User Identification")] *)
val login_name : unit -> string

(** [(describe-function 'user-real-login-name)]
    [(Info-goto-node "(elisp) User Identification")] *)
val real_login_name : unit -> string

(** [(describe-function 'system-users)]
    [(Info-goto-node "(elisp) User Identification")] *)
val system_user_names : unit -> string list

(** [(describe-function 'system-groups)]
    [(Info-goto-node "(elisp) User Identification")] *)
val system_group_names : unit -> string list

(** [(describe-function 'user-full-name)]
    [(Info-goto-node "(elisp) User Identification")] *)
val full_name : unit -> string

(** [(describe-function 'user-uid)]
    [(Info-goto-node "(elisp) User Identification")] *)
val uid : unit -> int

(** [(describe-function 'user-real-uid)]
    [(Info-goto-node "(elisp) User Identification")] *)
val real_uid : unit -> int

(** [(describe-function 'group-gid)]
    [(Info-goto-node "(elisp) User Identification")] *)
val gid : unit -> int

(** [(describe-function 'group-real-gid)]
    [(Info-goto-node "(elisp) User Identification")] *)
val real_gid : unit -> int

(** [(describe-variable 'user-init-file)]
    [(Info-goto-node "(elisp) Init File")]

    May be None during startup or if Emacs was started with `-q' or `--no-init-file'. *)
val init_file : Filename.t option Var.t
