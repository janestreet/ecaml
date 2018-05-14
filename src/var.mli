(** An ['a Var.t] is a symbol that holds values of type ['a], embedded in Elisp's
    [Value.t] via an ['a Type.t]. *)

open! Core_kernel
open! Import0

type 'a t =
  { symbol : Symbol.t
  ; type_  : 'a Value.Type.t }
[@@deriving fields, sexp_of]

type 'a var = 'a t

val create : Symbol.t -> 'a Value.Type.t -> 'a t

module And_value : sig
  type t = T : 'a var * 'a -> t
  [@@deriving sexp_of]
end

module And_value_option : sig
  type t = T : 'a var * 'a option -> t
  [@@deriving sexp_of]
end

val symbol_as_value : _ t -> Value.t

(** [(describe-function 'default-value)]
    [(Info-goto-node "(elisp)Default Value")] *)
val default_value_exn : 'a t -> 'a

(** [(describe-function 'default-boundp)]
    [(Info-goto-node "(elisp)Default Value")] *)
val default_value_is_defined : _ t -> bool

(** [(describe-function 'set-default)]
    [(Info-goto-node "(elisp)Default Value")] *)
val set_default_value : 'a t -> 'a -> unit

(** [(describe-function 'make-variable-buffer-local)]
    [(Info-goto-node "(elisp)Creating Buffer-Local")] *)
val make_buffer_local_always : _ t -> unit
