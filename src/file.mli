(** [File] is for functions that operate on files on the filesystem. *)

open! Core_kernel
open! Import

(** [(Info-goto-node "(elisp)Information about Files")] *)
val exists        : string -> bool (** [(describe-function 'file-exists-p)] *)
val is_directory  : string -> bool (** [(describe-function 'file-directory-p)] *)
val is_executable : string -> bool (** [(describe-function 'file-executable-p)] *)
val is_readable   : string -> bool (** [(describe-function 'file-readable-p)] *)
val is_regular    : string -> bool (** [(describe-function 'file-regular-p)] *)
val is_symlink    : string -> bool (** [(describe-function 'file-symlink-p)] *)
val is_writable   : string -> bool (** [(describe-function 'file-writable-p)] *)

(** [(describe-function 'file-in-directory-p)]
    [(Info-goto-node "(elisp)Truenames")] *)
val is_below : string -> dir:string -> bool

(** [(describe-function 'file-truename)]
    [(Info-goto-node "(elisp)Truenames")] *)
val truename : string -> string

(** [(describe-function 'delete-file)]
    [(Info-goto-node "(elisp)Changing Files")] *)
val delete : string -> unit

(** [(describe-function 'copy-file)]
    [(Info-goto-node "(elisp)Changing Files")] *)
val copy : src:string -> dst:string -> unit

(** [(describe-function 'rename-file)]
    [(Info-goto-node "(elisp)Changing Files")] *)
val rename : src:string -> dst:string -> replace_dst_if_exists:bool -> unit

(** [(describe-function 'locate-file)] *)
val locate
  :  ?suffixes:string list
  -> ?predicate:Value.t
  -> filename:string
  -> path:string list
  -> unit
  -> string option

(** [(describe-function 'locate-dominating-file)] *)
val locate_dominating_file
  :  above    : Filename.t
  -> basename : string
  -> Filename.t option

(** [(describe-function 'locate-dominating-file)] *)
val locate_dominating_file_exn
  :  above    : Filename.t
  -> basename : string
  -> Filename.t

(** [(describe-function 'write-region)]
    [(Info-goto-node "(elisp)Writing to Files")] *)
val write
  :  ?append : bool  (** default is [false] *)
  -> Filename.t
  -> string
  -> unit

(** [(describe-function 'make-temp-file)]
    [(Info-goto-node "(elisp)Unique File Names")] *)
val make_temp_file : prefix : string -> suffix : string -> Filename.t

(** Creates a temp file, calls [f] on its name, and deletes it after [f] returns, even if
    [f] returns by raising.

    N.B. This is not the behavior of [(describe-function 'with-temp-file)]. *)
val with_temp_file
  :  f : (Filename.t -> 'a)
  -> prefix : string
  -> suffix : string
  -> 'a
