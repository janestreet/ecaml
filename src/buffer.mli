(** Buffers are used to hold the contents of files that are being visited; there may also
    be buffers that are not visiting files.

    - [(Info-goto-node "(elisp)Buffers")] *)

open! Core_kernel
open! Import0
open! Ecaml_filename
open! Async_kernel
include Value.Subtype with type t = Buffer0.t

type buffer := t

include Equal.S with type t := t
module Compare_by_name : Comparable.S_plain with type t = t

(** Accessors *)

(** - [(describe-function 'buffer-file-name)] *)
val file_name : t -> string option

(** - [(describe-function 'buffer-live-p)   ] *)
val is_live : t -> bool

(** - [(describe-function 'buffer-name)     ] *)
val name : t -> string option

(** - [(describe-function 'get-buffer-process)] *)
val process : t -> Process0.t option

val process_exn : t -> Process0.t

(** [all_live] returns a list of all live buffers.  [(describe-function 'buffer-list)]. *)
val all_live : unit -> t list

(** [create ~name] creates a new buffer with name [name], adjusting the name if necessary
    to make the buffer's name unique.  [(describe-function 'generate-new-buffer)]. *)
val create : name:string -> t

(** [find ~name] returns the live buffer whose name is [name], if any.
    [(describe-function 'get-buffer)]. *)
val find : name:string -> t option

val find_exn : name:string -> t

(** [(describe-function 'get-file-buffer)].
    [(Info-goto-node "(elisp)Buffer File Name")] *)
val find_visiting : file:Filename.t -> t option

(** [find ~name] returns the live buffer whose name is [name], and if there is no such
    buffer, creates it.  [(describe-function 'get-buffer-create)]. *)
val find_or_create : name:string -> t

(** [kill t] kills [t], so that [not (is_live t)].  [(describe-function 'kill-buffer)]. *)
val kill : t -> unit Deferred.t

module Blocking : sig
  val kill : t -> unit
end

(** Return a list of all windows on the current terminal that are displaying the given buffer.

    [(describe-function 'get-buffer-window-list)] *)
val displayed_in : ?current_frame_only:bool -> t -> Window0.t list

(** [(Info-goto-node "(elisp)Choosing Window")]
    [(describe-function 'display-buffer)] *)
val display : t -> Window0.t option

(** Like [display], but ignores the result. *)
val display_i : t -> unit

(** [(describe-function 'buffer-local-value)]
    [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val buffer_local_value : t -> 'a Var.t -> 'a

(** [(describe-function 'buffer-local-variables)]
    [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val buffer_local_variables : t -> (Symbol.t * Value.t option) list

(** [(describe-function 'find-file-noselect)]
    [(Info-goto-node "(elisp)Visiting Functions")]

    See also [Selected_window.find_file]. *)
val find_file_noselect : Filename.t -> t Deferred.t

(** From [(Info-goto-node "(emacs)Select Buffer")]:

    Emacs uses buffer names that start with a space for internal purposes. It treats these
    buffers specially in minor ways---for example, by default they do not record undo
    information. It is best to avoid using such buffer names yourself.

    As dead buffers have no names, it is unknown whether a dead buffer was internal. *)
val is_internal_or_dead : t -> bool

module Which_buffers : sig
  type t =
    | File_visiting
    | These of (buffer -> bool)
  [@@deriving sexp_of]
end

(** [(describe-function 'save-some-buffers)]
    [(Info-goto-node "(elisp)Saving Buffers")] *)
val save_some
  :  ?query:bool (** default is [true] *)
  -> ?which_buffers:Which_buffers.t (** default is [File_visiting] *)
  -> unit
  -> unit Deferred.t

val with_temp_buffer : (t -> 'a Deferred.t) -> 'a Deferred.t

(** [(describe-function 'revert-buffer)]
    [(Info-goto-node "(elisp)Reverting")] *)
val revert : ?confirm:bool (** default is [false] *) -> t -> unit Deferred.t

(** [(describe-variable 'kill-buffer-query-functions)] *)
val kill_buffer_query_functions : Function.t list Var.t

(** [(describe-function 'buffer-modified-tick)] *)
val modified_tick : t -> Modified_tick.t

(** [(describe-function 'buffer-chars-modified-tick)] *)
val chars_modified_tick : t -> Modified_tick.t
