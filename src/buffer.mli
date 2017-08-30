(** Buffers are used to hold the contents of files that are being visited; there may also
    be buffers that are not visiting files.

    - [(Info-goto-node "(elisp)Buffers")] *)

open! Core_kernel
open! Import

type t [@@deriving sexp_of]

include Equal.S       with type t := t
include Value.Subtype with type t := t

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

module Private : sig
  val current : unit -> t
end
