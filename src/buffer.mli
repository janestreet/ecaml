(** Buffers are used to hold the contents of files that are being visited; there may also
    be buffers that are not visiting files.

    - [(Info-goto-node "(elisp)Buffers")] *)

open! Core_kernel
open! Import0

include Value.Subtype with type t = Buffer0.t

type buffer = t

include Equal.S with type t := t

(** Accessors *)
val file_name : t -> string option     (** - [(describe-function 'buffer-file-name)] *)
val is_live   : t -> bool              (** - [(describe-function 'buffer-live-p)   ] *)
val name      : t -> string option     (** - [(describe-function 'buffer-name)     ] *)
val process   : t -> Process0.t option (** - [(describe-function 'get-buffer-process)] *)

(** [all_live] returns a list of all live buffers.  [(describe-function 'buffer-list)]. *)
val all_live : unit -> t list

(** [create ~name] creates a new buffer with name [name], adjusting the name if necessary
    to make the buffer's name unique.  [(describe-function 'generate-new-buffer)]. *)
val create : name : string -> t

(** [find ~name] returns the live buffer whose name is [name], if any.
    [(describe-function 'get-buffer)]. *)
val find : name : string -> t option

(** [(describe-function 'get-file-buffer)].
    [(Info-goto-node "(elisp)Buffer File Name")] *)
val find_visiting : file : Filename.t -> t option

(** [find ~name] returns the live buffer whose name is [name], and if there is no such
    buffer, creates it.  [(describe-function 'get-buffer-create)]. *)
val find_or_create : name : string -> t

(** [kill t] kills [t], so that [not (is_live t)].  [(describe-function 'kill-buffer)]. *)
val kill : t -> unit

(** [(describe-function 'get-buffer-window-list)] *)
val displayed_in : t -> Window0.t list

(** [(Info-goto-node "(elisp)Choosing Window")]
    [(describe-function 'display-buffer)] *)
val display : t -> unit

(** [(describe-function 'buffer-local-value)]
    [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val buffer_local_value : t -> 'a Var.t -> 'a

(** [(describe-function 'buffer-local-variables)]
    [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val buffer_local_variables : t -> (Symbol.t * Value.t option) list

(** [(describe-function 'find-file-noselect)]
    [(Info-goto-node "(elisp)Visiting Functions")]

    See also [Selected_window.find_file].
*)
val find_file_noselect : Filename.t -> t

module Which_buffers : sig
  type t =
    | File_visiting
    | These of (buffer -> bool)
  [@@deriving sexp_of]
end

(** [(describe-function 'save-some-buffers)]
    [(Info-goto-node "(elisp)Saving Buffers")] *)
val save_some
  :  ?query : bool  (** default is [true] *)
  -> ?which_buffers : Which_buffers.t  (** default is [File_visiting] *)
  -> unit
  -> unit
