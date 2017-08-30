open! Core_kernel
open! Import

type t =
  | Bool
  | Float
  | Ignore  (** Ignore return value. Cannot be used as an input type. *)
  | Int
  | Value
;;
