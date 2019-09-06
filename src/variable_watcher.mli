(** A variable watcher provides a way to run a function when the value of a variable is
    changed.

    Requires emacs 26.1 or later.

    [(Info-goto-node "(elisp)Watching Variables")]
*)

open! Core_kernel
open! Import

module Operation : sig
  type t =
    | Set
    | Let
    | Unlet
    | Makunbound
    | Defvaralias
  [@@deriving sexp_of]
end

module Event : sig
  type t =
    { local_to_buffer : Buffer.t option
    ; new_value : Value.t
    ; operation : Operation.t
    ; variable_changed : Symbol.t
    }
  [@@deriving sexp_of]
end

type t

(** [(describe-function 'add-variable-watcher)] *)
val add : Source_code_position.t -> 'a Var.t -> f:(Event.t -> unit) -> t

(** [(describe-function 'remove-variable-watcher)] *)
val remove : t -> unit
