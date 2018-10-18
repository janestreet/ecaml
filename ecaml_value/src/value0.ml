open! Core_kernel
open! Import

type t

let sexp_of_t_ref = ref (fun _ -> [%message [%here] "sexp_of_t not yet defined"])
let sexp_of_t t = !sexp_of_t_ref t
