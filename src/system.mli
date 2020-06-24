(**  [(Info-goto-node "(elisp)System Environment")]

     Emacs maintains its own copy of the path and environment which is not visible to
     OCaml. Therefore, [Async.Process.create] will not give the same path and environment
     as [Ecaml.Process.create], and [Ecaml.System.setenv ~var:"PATH"] does not affect the
     behavior of [Async.Process.create] (or [Ecaml.Process.create]!). *)

open! Core_kernel
open! Import

(** [(describe-function 'getenv)]
    [(Info-goto-node "(elisp)System Environment")] *)
val getenv : var:string -> string option

(** [(describe-function 'setenv)]
    [(Info-goto-node "(elisp)System Environment")] *)
val setenv : var:string -> value:string option -> unit

(** [(describe-variable 'process-environment)]
    [(Info-goto-node "(elisp)System Environment")] *)
val process_environment : string list Var.t

(** [(describe-variable 'noninteractive)]
    [(Info-goto-node "(elisp)Batch Mode")] *)
val noninteractive : bool Var.t

(** [is_interactive () = not (Current_buffer.value_exn noninteractive)] *)
val is_interactive : unit -> bool

val hostname : unit -> string

(** [(describe-variable 'exec-path)] *)
val exec_path : string list Customization.t

module Var_and_value : sig
  type t =
    { var : string
    ; value : string
    }
  [@@deriving sexp_of]
end

val setenv_temporarily
  :  (_, 'a) Sync_or_async.t
  -> Var_and_value.t list
  -> f:(unit -> 'a)
  -> 'a
