open! Core_kernel
open! Import
module Id = Caml_embedded_id

type t = Value.t [@@deriving sexp_of]

external ecaml_inject : Id.t -> t = "ecaml_inject"
external ecaml_project : t -> Id.t = "ecaml_project"

let ecaml_project t =
  try ecaml_project t with
  | exn ->
    let name = "[Caml_embed.ecaml_project]" in
    raise_s
      (match exn with
       | Failure message -> [%message (concat [ name; ": "; message ]) ~_:(t : t)]
       | _ -> [%message name ~_:(exn : exn) ~_:(t : t)])
;;

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
  else failwith "Ecaml value doesn't represent ocaml embedded value"
;;

let () =
  Ecaml_callback.(register free_embedded_caml_values)
    [%here]
    ~f:(fun ids -> Array.iter ids ~f:(fun id -> Hashtbl.remove embedded_values id))
    ~should_run_holding_async_lock:true
;;

let debug_sexp () = [%sexp_of: Univ.t Id.Table.t] embedded_values

let create_type (type_id : _ Type_equal.Id.t) =
  Value.Type.create
    [%message "caml_embed" ~type_id:(Type_equal.Id.name type_id : string)]
    (Type_equal.Id.to_sexp type_id)
    (fun v ->
       let embed = of_value_exn v in
       extract_exn embed type_id)
    (fun a -> create type_id a |> to_value)
;;
