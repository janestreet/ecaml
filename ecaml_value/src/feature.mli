open! Core_kernel
open! Import

type t = Symbol.t

(** [(describe-function 'require)].  [require] has its own OCaml-side table that avoids
    repeated calls to Elisp's [require] on the same symbol.*)
val require : t -> unit
