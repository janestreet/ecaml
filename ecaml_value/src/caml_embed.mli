open! Core_kernel
open! Import

type t

val lookup_by_id_exn : Caml_embedded_id.t -> 'a Type_equal.Id.t -> 'a
val debug_sexp : unit -> Sexp.t

(** Embed values of an arbitrary OCaml type ['a] in an Elisp value.  The values are not
    transformed, so this can be used to preserve state in Emacs.  More precisely, the
    following returns [true]:

    {[
      let var = Var.create symbol (Caml_embed.create_type type_id) in
      Current_buffer.set_value var v;
      phys_equal v (Current_buffer.value_exn var)
    ]} *)
val create_type : 'a Type_equal.Id.t -> 'a Value.Type.t
