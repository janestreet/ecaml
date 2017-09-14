open! Core_kernel
open! Import

type t
  =  ?ocaml_name:string
  -> string
  -> Type.t list  (** input types *)
  -> Type.t       (** return type *)
  -> unit

val c     : unit -> t
val ocaml : t
