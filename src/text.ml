open! Core
open! Import

module Q = struct
  include Q

  let after_string = "after-string" |> Symbol.intern
  let background_color = "background-color" |> Symbol.intern
  let before_string = "before-string" |> Symbol.intern
  let concat = "concat" |> Symbol.intern
  let display = "display" |> Symbol.intern
  let font_lock_face = "font-lock-face" |> Symbol.intern
  let foreground_color = "foreground-color" |> Symbol.intern
  let invisible = "invisible" |> Symbol.intern
  let mouse_face = "mouse-face" |> Symbol.intern
  let propertize = "propertize" |> Symbol.intern
  let help_echo = "help-echo" |> Symbol.intern
  let kbd_help = "kbd-help" |> Symbol.intern
  let string = "string" |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "text"
    let here = [%here]
    let is_in_subtype = Value.is_string
  end)

let char_code = Funcall.Wrap.("aref" <: t @-> int @-> return Char_code.t)
let set_char_code = Funcall.Wrap.("aset" <: t @-> int @-> Char_code.t @-> return nil)
let of_utf8_bytes string = string |> Value.of_utf8_bytes |> of_value_exn

let of_utf8_bytes_replacing_invalid string =
  string |> Value.of_utf8_bytes_replacing_invalid |> Tuple2.map_fst ~f:of_value_exn
;;

let to_utf8_bytes t = t |> to_value |> Value.to_utf8_bytes_exn

module Compare_as_string = struct
  module T0 = struct
    type nonrec t = t

    let compare a b = Comparable.lift [%compare: string] ~f:to_utf8_bytes a b
    let of_string = of_utf8_bytes
    let to_string = to_utf8_bytes
  end

  module T = struct
    include T0
    include Sexpable.Of_stringable (T0)
  end

  include T
  include Comparable.Make (T)
end

let length = Funcall.Wrap.("length" <: t @-> return int)
let concat ts = Symbol.funcallN Q.concat (ts : t list :> Value.t list) |> of_value_exn
let string_join = Funcall.Wrap.("string-join" <: list t @-> t @-> return t)

let concat ?sep ts =
  match sep with
  | Some sep -> string_join ts sep
  | None -> concat ts
;;

let substring =
  let substring = Funcall.Wrap.("substring" <: t @-> int @-> int @-> return t) in
  fun t ~start ~end_ -> substring t start end_
;;

module Face_spec = struct
  module One = struct
    type t =
      | Attributes of Face.Attribute_and_value.t list
      | Face of Face.t
    [@@deriving sexp_of]

    let normalize = function
      | Face _ as t -> t
      | Attributes attributes ->
        Attributes (attributes |> Face.Attribute_and_value.sort_by_attribute_name)
    ;;

    let compare t1 t2 =
      match t1, t2 with
      | Face face1, Face face2 -> String.compare (Face.to_name face1) (Face.to_name face2)
      | Attributes a1, Attributes a2 ->
        List.compare Face.Attribute_and_value.compare_attribute_name a1 a2
      | Face _, _ -> -1
      | _, Face _ -> 1
    ;;

    let to_value (t : t) : Value.t =
      match t with
      | Attributes attributes ->
        Value.list
          (List.fold
             (List.rev attributes)
             ~init:[]
             ~f:(fun ac (Face.Attribute_and_value.T (attribute, value)) ->
               (attribute |> Face.Attribute.to_symbol |> Symbol.to_value)
               :: (value |> Face.Attribute.to_value attribute)
               :: ac))
      | Face face -> face |> Face.to_value
    ;;

    let raise_unexpected value =
      raise_s [%message "[Face.One.of_value_exn] got unexpected value" (value : Value.t)]
    ;;

    let of_value_exn value : t =
      match Face.of_value_exn value with
      | x -> Face x
      | exception _ ->
        if not (Value.is_cons value) then raise_unexpected value;
        let car = Value.car_exn value in
        let cdr = Value.cdr_exn value in
        if not (Value.is_cons cdr)
        then (
          (* Old style specs: [(background-color . color)] [(foreground-color . color)] *)
          let symbol = car |> Symbol.of_value_exn in
          let color = cdr |> Color.of_value_exn in
          if Symbol.equal symbol Q.foreground_color
          then Attributes [ T (Foreground, Color color) ]
          else if Symbol.equal symbol Q.background_color
          then Attributes [ T (Background, Color color) ]
          else raise_unexpected value)
        else (
          let rec loop value ac =
            if Value.is_nil value
            then Attributes (List.rev ac)
            else (
              if not (Value.is_cons value) then raise_unexpected value;
              let car = Value.car_exn value in
              let cdr = Value.cdr_exn value in
              if Value.is_cons car
              then loop cdr ((car |> Face.Attribute_and_value.of_value_exn) :: ac)
              else
                let module A = Face.Attribute.Packed in
                let (A.T attribute) = car |> A.of_value_exn in
                loop
                  (Value.cdr_exn cdr)
                  (T
                     ( attribute
                     , Value.car_exn cdr |> Face.Attribute.of_value_exn attribute )
                   :: ac))
          in
          loop value [])
    ;;

    let type_ =
      Value.Type.create [%sexp "text-face-spec-one"] [%sexp_of: t] of_value_exn to_value
    ;;

    let t = type_
  end

  type t = One.t list [@@deriving sexp_of]

  let normalize t = t |> List.map ~f:One.normalize |> List.sort ~compare:One.compare

  let to_value t =
    match t with
    | [] -> Value.nil
    | [ one ] -> One.to_value one
    | _ -> Value.list (List.map t ~f:One.to_value)
  ;;

  let of_value_exn value =
    if Value.is_nil value
    then []
    else (
      match One.of_value_exn value with
      | one -> [ one ]
      | exception _ -> Value.to_list_exn value ~f:One.of_value_exn)
  ;;

  let of_value_exn value =
    try of_value_exn value with
    | exn ->
      raise_s
        [%message
          "[Text.Face_spec.of_value_exn] got unexpected value"
            (value : Value.t)
            (exn : exn)]
  ;;

  let type_ =
    Value.Type.create [%sexp "text-face-spec"] [%sexp_of: t] of_value_exn to_value
  ;;

  let t = type_
end

module Display_spec = struct
  type nonrec t =
    { property : Display_property.t
    ; text : t
    }
  [@@deriving sexp_of]

  let to_value (t : t) : Value.t =
    Value.list [ Display_property.to_values t.property |> Value.list; t.text |> to_value ]
  ;;

  (* We expect values to be of the form ['((margin MARGIN) TEXT)]. *)
  let of_value_exn value : t =
    match Value.to_list_exn value ~f:Fn.id with
    | [] | [ _ ] | _ :: _ :: _ :: _ ->
      raise_s [%sexp "Display_spec: Could not convert value", (value : Value.t)]
    | [ prop; txt ] ->
      { property =
          Display_property.of_values_exn
            (Value.car_exn prop, Value.car_exn (Value.cdr_exn prop))
      ; text = txt |> of_value_exn
      }
  ;;

  let type_ =
    Value.Type.create [%sexp "text-display-spec"] [%sexp_of: t] of_value_exn to_value
  ;;
end

open struct
  let text_type = type_
end

module Property_name = struct
  type 'a t =
    { name : Symbol.t
    ; type_ : 'a Value.Type.t
    }
  [@@deriving fields ~getters]

  type 'a property_name = 'a t

  let name_as_value t = t |> name |> Symbol.to_value
  let sexp_of_t _ t = [%sexp (name t : Symbol.t)]
  let of_value_exn t = Value.Type.of_value_exn (type_ t)
  let to_value t = Value.Type.to_value (type_ t)
  let value_to_sexp t = Value.Type.to_sexp (type_ t)

  module Packed = struct
    type t = T : _ property_name -> t

    let sexp_of_t (T p) = [%sexp (p : _ t)]
    let name (T p) = name p
    let all_except_unknown = ref []

    let of_name_as_value_exn value =
      match Symbol.of_value_exn value with
      | exception _ ->
        raise_s
          [%message
            "[Text.Property.Packed.of_name_as_value_exn] got unexpected value"
              (value : Value.t)]
      | symbol ->
        (match
           List.find !all_except_unknown ~f:(fun t -> Symbol.equal symbol (name t))
         with
         | Some t -> t
         | None -> T { name = symbol; type_ = Value.Type.value })
    ;;
  end

  let create_and_register name type_ =
    let t = { name; type_ } in
    Packed.all_except_unknown := T t :: !Packed.all_except_unknown;
    t
  ;;

  module Create = struct
    let ( <: ) name type_ = create_and_register (Symbol.intern name) type_
  end

  let face : _ t = create_and_register Q.face Face_spec.type_
  let mouse_face : _ t = create_and_register Q.mouse_face Face_spec.type_
  let font_lock_face : _ t = create_and_register Q.font_lock_face Face_spec.type_
  let display : _ t = create_and_register Q.display Display_spec.type_
  let after_string : _ t = create_and_register Q.after_string text_type
  let before_string : _ t = create_and_register Q.before_string text_type
  let invisible : _ t = create_and_register Q.invisible Value.Type.value
  let help_echo : _ t = create_and_register Q.help_echo Value.Type.string
  let kbd_help : _ t = create_and_register Q.kbd_help Value.Type.string
end

module Property = struct
  type t = T : 'a Property_name.t * 'a -> t

  let sexp_of_t (T (property_name, property_value)) =
    [%message
      ""
        ~_:(Property_name.name property_name : Symbol.t)
        ~_:(Property_name.value_to_sexp property_name property_value : Sexp.t)]
  ;;

  let rec of_property_list_exn value =
    if Value.is_nil value
    then []
    else if not (Value.is_cons value)
    then
      raise_s
        [%message
          "[Text.Property.of_property_list_exn] got unexpected value" (value : Value.t)]
    else
      let module N = Property_name.Packed in
      let (N.T property_name) = Value.car_exn value |> N.of_name_as_value_exn in
      let property_value_and_rest = Value.cdr_exn value in
      T
        ( property_name
        , Value.car_exn property_value_and_rest
          |> Property_name.of_value_exn property_name )
      :: of_property_list_exn (Value.cdr_exn property_value_and_rest)
  ;;

  let to_property_list ts =
    List.fold (List.rev ts) ~init:[] ~f:(fun ac (T (name, value)) ->
      let type_ = Property_name.type_ name in
      (Property_name.name name |> Symbol.to_value)
      :: (value |> Value.Type.to_value type_)
      :: ac)
  ;;
end

let propertize t properties =
  Symbol.funcallN
    Q.propertize
    ((t |> to_value) :: (properties |> Property.to_property_list))
  |> of_value_exn
;;

let colorize t ~color =
  propertize t [ T (Property_name.face, [ Attributes [ T (Foreground, Color color) ] ]) ]
;;

let get_text_property =
  Funcall.Wrap.("get-text-property" <: int @-> Symbol.t @-> t @-> return value)
;;

let property_value t ~at property_name =
  let value = get_text_property at (property_name |> Property_name.name) t in
  if Value.is_nil value
  then None
  else Some (value |> Property_name.of_value_exn property_name)
;;

let text_properties_at = Funcall.Wrap.("text-properties-at" <: int @-> t @-> return value)
let properties t ~at = text_properties_at at t |> Property.of_property_list_exn

let get_start start =
  match start with
  | Some i -> i
  | None -> 0
;;

let get_end t end_ =
  match end_ with
  | Some i -> i
  | None -> length t
;;

let put_text_property =
  Funcall.Wrap.(
    "put-text-property" <: int @-> int @-> Symbol.t @-> value @-> t @-> return nil)
;;

let set_property ?start ?end_ t property_name property_value =
  put_text_property
    (start |> get_start)
    (end_ |> get_end t)
    (property_name |> Property_name.name)
    (property_value |> Property_name.to_value property_name)
    t
;;

let add_text_properties =
  Funcall.Wrap.("add-text-properties" <: int @-> int @-> list value @-> t @-> return nil)
;;

let add_properties ?start ?end_ t properties =
  add_text_properties
    (start |> get_start)
    (end_ |> get_end t)
    (properties |> Property.to_property_list)
    t
;;

let add_face_text_property =
  Funcall.Wrap.(
    "add-face-text-property" <: int @-> int @-> Face_spec.t @-> bool @-> t @-> return nil)
;;

let add_face_properties ?start ?end_ ?(append = false) t ~spec =
  add_face_text_property (start |> get_start) (end_ |> get_end t) spec append t;
  t
;;

let set_text_properties =
  Funcall.Wrap.("set-text-properties" <: int @-> int @-> list value @-> t @-> return nil)
;;

let set_properties ?start ?end_ t properties =
  set_text_properties
    (start |> get_start)
    (end_ |> get_end t)
    (properties |> Property.to_property_list)
    t
;;

let remove_list_of_text_properties =
  Funcall.Wrap.(
    "remove-list-of-text-properties" <: int @-> int @-> list Symbol.t @-> t @-> return nil)
;;

let remove_properties ?start ?end_ t property_names =
  remove_list_of_text_properties
    (start |> get_start)
    (end_ |> get_end t)
    (property_names |> List.map ~f:Property_name.Packed.name)
    t
;;

let is_multibyte = Funcall.Wrap.("multibyte-string-p" <: t @-> return bool)
let num_bytes = Funcall.Wrap.("string-bytes" <: t @-> return int)
let to_multibyte = Funcall.Wrap.("string-to-multibyte" <: t @-> return t)
let to_unibyte_exn = Funcall.Wrap.("string-to-unibyte" <: t @-> return t)

let of_char_array chars =
  Symbol.funcallN_array Q.string (Array.map chars ~f:Char_code.to_value) |> of_value_exn
;;

external to_char_array : t -> Char_code.t array = "ecaml_text_to_char_array"

let string_search = Funcall.Wrap.("string-search" <: string @-> t @-> return (nil_or int))
let string_search ~needle t = string_search needle t
