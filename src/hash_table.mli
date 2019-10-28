(** [(Info-goto-node "(elisp)Hash Tables")] *)

open! Core_kernel
open! Import
include Value.Subtype

module Test : sig
  type t =
    | Eq (** [(describe-function 'eq)] *)
    | Eql (** [(describe-function 'eql)] *)
    | Equal (** [(describe-function 'equal)] *)
end

(** [(describe-function 'make-hash-table)]
    [(Info-goto-node "(elisp) Creating Hash")] *)
val create : ?test:Test.t (** default is [Eql] *) -> unit -> t

(** [(describe-function 'gethash)] *)
val find : t -> Value.t -> Value.t option

(** [(describe-function 'puthash)] *)
val set : t -> key:Value.t -> data:Value.t -> unit

(** [(describe-function 'remhash)] *)
val remove : t -> Value.t -> unit

(** [(describe-function 'hash-table-size)] *)
val length : t -> int

(** [(describe-function 'hash-table-keys)] *)
val keys : t -> Value.t list
