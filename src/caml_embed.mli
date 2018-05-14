open! Core_kernel
open! Import0

type t

val create : 'a Type_equal.Id.t -> 'a -> t

val extract_exn : t -> 'a Type_equal.Id.t -> 'a

val lookup_by_id_exn : Caml_embedded_id.t -> 'a Type_equal.Id.t -> 'a

val to_value : t -> Value0.t
val of_value_exn : Value0.t -> t

val initialize : unit

val debug_sexp : unit -> Sexp.t
