open! Core
open! Import

module Field = struct
  type 'a t =
    { name : string
    ; to_value : 'a -> Value.t
    (* [to_value] extracts the field from the corresponding OCaml record and serializes it
       to Elisp. *)
    }

  let build_form field = Form.symbol (Symbol.intern field.name)

  let field (name : string) (to_value : 'a -> 'b) (type_ : 'b Value.Type.t) =
    { name; to_value = (fun entry -> Value.Type.to_value type_ (to_value entry)) }
  ;;

  include (Value.Type : Value.Type.S)
end

module Q = struct
  let cl_defstruct = "cl-defstruct" |> Symbol.intern
end

let build_form ~name ~doc fields =
  Form.list
    (Form.symbol Q.cl_defstruct
     :: Form.symbol (Symbol.intern name)
     :: Form.string doc
     :: List.map ~f:Field.build_form fields)
;;

type 'a t =
  { maker : Symbol.t
  ; args : (Symbol.t * ('a -> Value.t)) list
  ; defined_by_feature : Feature.t option
  }

let make t a =
  Option.iter ~f:Feature.require t.defined_by_feature;
  Symbol.funcallN
    t.maker
    (List.concat_map
       ~f:(fun (field, to_value) -> [ Symbol.to_value field; to_value a ])
       t.args)
;;

module For_testing = struct
  module Packed = struct
    type nonrec 'a defstruct = 'a t
    type t = T : string * 'a defstruct -> t
  end

  let defstructs : Packed.t list ref = ref []

  let all_defstructs () =
    !defstructs
    |> List.map ~f:(fun (T (name, t)) -> name, t.maker, t.args |> List.map ~f:fst)
    |> List.sort ~compare:(Comparable.lift [%compare: string] ~f:fst3)
  ;;
end

let defstruct
  ~(here : [%call_pos])
  ~(name : string)
  ?defined_by_feature
  ~(doc : string)
  (fields : 'a Field.t list)
  =
  let t =
    { maker = Symbol.intern ("make-" ^ name)
    ; args =
        List.map ~f:(fun field -> Symbol.intern (":" ^ field.name), field.to_value) fields
    ; defined_by_feature
    }
  in
  if Option.is_none defined_by_feature
  then (
    Dump.eval_and_dump ~here (fun () -> build_form ~name ~doc fields);
    (* This defstruct wrapper doesn't support customizing the predicate or constructor
       names, or anything like that, so we can predict all of the names [cl-defstruct]
       will define. *)
    List.iter
      (List.concat
         [ [ [%string "make-%{name}"]; [%string "copy-%{name}"]; [%string "%{name}-p"] ]
         ; List.map fields ~f:(fun field -> [%string "%{name}-%{field.name}"])
         ])
      ~f:(fun symbol -> Load_history.add_entry here (Fun (Symbol.intern symbol)));
    For_testing.defstructs := T (name, t) :: !For_testing.defstructs);
  t
;;
