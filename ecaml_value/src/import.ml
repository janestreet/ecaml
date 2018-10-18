open! Core_kernel

let concat = String.concat
let eprint_s = Core_kernel.Debug.eprint_s

(* included last so it can't be shadowed *)
include Int.Replace_polymorphic_compare
