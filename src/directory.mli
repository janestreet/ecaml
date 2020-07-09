open! Core_kernel
open! Import
open! Ecaml_filename

(** [(describe-function 'make-directory)]
    [(Info-goto-node "(elisp)Create/Delete Dirs")] *)
val create : ?parents:bool (** default is [false] *) -> Filename.t -> unit

(** [(describe-function 'delete-directory)]
    [(Info-goto-node "(elisp)Create/Delete Dirs")] *)
val delete : ?recursive:bool (** default is [false] *) -> Filename.t -> unit

(** [(describe-function 'directory-files)]
    [(Info-goto-node "(elisp)Contents of Directories")]*)
val files
  :  ?absolute:bool (** default is [false] *)
  -> ?include_dot_and_dotdot:bool (** default is [false] *)
  -> ?matching:Regexp.t
  -> ?sort:bool (** default is [true] *)
  -> Filename.t
  -> Filename.t list

(** [(describe-function 'directory-files-recursively)]
    [(Info-goto-node "(elisp)Contents of Directories")]*)
val files_recursively
  :  ?include_directories:bool (** default is [false] *)
  -> ?matching:Regexp.t (** default is [Regexp.match_anything] *)
  -> Filename.t
  -> Filename.t list

(** [(describe-function 'make-temp-file)]
    [(Info-goto-node "(elisp)Unique File Names")] *)
val make_temp_dir : prefix:string -> suffix:string -> Filename.t

(** Creates a temp directory, calls [f] on its name, and deletes it after [f] returns,
    even if [f] returns by raising.

    N.B. This is not the behavior of [(describe-function 'with-temp-file)]. *)
val with_temp_dir
  :  (_, 'a) Sync_or_async.t
  -> f:(Filename.t -> 'a)
  -> prefix:string
  -> suffix:string
  -> 'a

(** The directory to use for temporary files.
    [(describe-variable 'temporary-file-directory)].  *)
val for_temp_files_customization : Filename.t Customization.t

(** [for_temp_files () = Customization.value for_temp_files_customization] *)
val for_temp_files : unit -> Filename.t

(** You probably want [for_temp_files].  Beware, if the current buffer is a tramp buffer
    on a remote host, then the temporary file directory will be on the remote host.  This
    function is only available starting in emacs 26.  In earlier versions, this reads the
    variable [temporary-file-directory].
    [(describe-function 'temporary-file-directory)]. *)
val for_temp_files_of_current_buffer : unit -> Filename.t
