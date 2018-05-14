open! Core_kernel
open! Import0

module Id = Caml_embedded_id

type t = Value0.t

external ecaml_inject  : Id.t -> t    = "ecaml_inject"
external ecaml_project : t    -> Id.t = "ecaml_project"

let embedded_values = Id.Table.create ()

let create type_id v =
  let id = Id.create () in
  Hashtbl.set embedded_values ~key:id ~data:(Univ.create type_id v);
  ecaml_inject id
;;

let lookup_by_id_exn id type_id =
  let univ = Hashtbl.find_exn embedded_values id in
  Univ.match_exn univ type_id
;;

let extract_exn t type_id =
  let id = ecaml_project t in
  lookup_by_id_exn id type_id
;;

let to_value = Fn.id

let of_value_exn v =
  let id = ecaml_project v in
  if Hashtbl.mem embedded_values id
  then v
  else
    failwith "Ecaml value doesn't represent ocaml embedded value"
;;

let initialize =
  Ecaml_callback.(register free_embedded_caml_values)
    ~f:(fun ids ->
      Array.iter ids ~f:(fun id -> Hashtbl.remove embedded_values id))
;;

let debug_sexp () = [%sexp_of: Univ.t Id.Table.t] embedded_values
