open! Core_kernel
open! Import

(** [(describe-function 'make-directory)]
    [(Info-goto-node "(elisp)Create/Delete Dirs")] *)
val create
  :  ?parents : bool  (** default is [false] *)
  -> Filename.t
  -> unit

(** [(describe-function 'delete-directory)]
    [(Info-goto-node "(elisp)Create/Delete Dirs")] *)
val delete
  :  ?recursive : bool  (** default is [false] *)
  -> Filename.t
  -> unit

(** [(describe-function 'directory-files)]
    [(Info-goto-node "(elisp)Contents of Directories")]*)
val files
  :  ?absolute               : bool     (** default is [false] *)
  -> ?include_dot_and_dotdot : bool     (** default is [false] *)
  -> ?matching               : Regexp.t
  -> ?sort                   : bool     (** default is [true] *)
  -> Filename.t
  -> Filename.t list

(** [(describe-function 'directory-files-recursively)]
    [(Info-goto-node "(elisp)Contents of Directories")]*)
val files_recursively
  :  ?include_directories : bool      (** default is [false] *)
  -> ?matching            : Regexp.t  (** default is [Regexp.match_anything] *)
  -> Filename.t
  -> Filename.t list
