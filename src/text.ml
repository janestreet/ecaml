open! Core_kernel
open! Import

module Q = struct
  include Q
  let background_color                 = "background-color"                 |> Symbol.intern
  let concat                           = "concat"                           |> Symbol.intern
  let font_lock_face                   = "font-lock-face"                   |> Symbol.intern
  let foreground_color                 = "foreground-color"                 |> Symbol.intern
  let get_text_property                = "get-text-property"                |> Symbol.intern
  let multibyte_string_p               = "multibyte-string-p"               |> Symbol.intern
  let propertize                       = "propertize"                       |> Symbol.intern
  let remove_list_of_text_properties   = "remove-list-of-text-properties"   |> Symbol.intern
  let string_bytes                     = "string-bytes"                     |> Symbol.intern
  let string_to_multibyte              = "string-to-multibyte"              |> Symbol.intern
  let string_to_unibyte                = "string-to-unibyte"                |> Symbol.intern
  let text_properties_at               = "text-properties-at"               |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "text"
    let here = [%here]
    let is_in_subtype = Value.is_string
  end)

let char_code t i =
  Symbol.funcall2 Q.aref (t |> to_value) (i |> Value.of_int_exn)
  |> Char_code.of_value_exn
;;

let set_char_code t i char_code =
  Symbol.funcall3_i Q.aset
    (t |> to_value)
    (i |> Value.of_int_exn)
    (char_code |> Char_code.to_value)
;;

let of_utf8_bytes string = string |> Value.of_utf8_bytes |> of_value_exn

let to_utf8_bytes t = t |> to_value |> Value.to_utf8_bytes_exn

module Compare_as_string = struct
  module T0 = struct
    type nonrec t = t
    let compare = Comparable.lift [%compare: string] ~f:to_utf8_bytes
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

let length t = Symbol.funcall1 Q.length (t |> to_value) |> Value.to_int_exn

let concat ts = Symbol.funcallN Q.concat (ts : t list :> Value.t list) |> of_value_exn

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
      | Face face1, Face face2 ->
        String.compare (Face.to_name face1) (Face.to_name face2)
      | Attributes a1, Attributes a2 ->
        List.compare Face.Attribute_and_value.compare_attribute_name a1 a2
      | Face _ , _      -> -1
      | _      , Face _ ->  1
    ;;

    let to_value (t : t) : Value.t =
      match t with
      | Attributes attributes ->
        Value.list (
          List.fold (List.rev attributes) ~init:[]
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
              else (
                let module A = Face.Attribute.Packed in
                let A.T attribute = car |> A.of_value_exn in
                loop (Value.cdr_exn cdr)
                  (T (attribute,
                      Value.car_exn cdr
                      |> Face.Attribute.of_value_exn attribute)
                   :: ac))) in
          loop value [])
    ;;
  end

  type t = One.t list [@@deriving sexp_of]

  let normalize t =
    t
    |> List.map ~f:One.normalize
    |> List.sort ~compare:One.compare
  ;;

  let to_value t =
    match t with
    | [] -> Value.nil
    | [ one ] -> One.to_value one
    | _ -> Value.list (List.map t ~f:One.to_value)
  ;;

  let of_value_exn value =
    if Value.is_nil value
    then []
    else
      match One.of_value_exn value with
      | one -> [ one ]
      | exception _ -> Value.to_list_exn value ~f:One.of_value_exn
  ;;

  let of_value_exn value =
    try
      of_value_exn value
    with exn ->
      raise_s [%message
        "[Text.Face_spec.of_value_exn] got unexpected value"
          (value : Value.t)
          (exn : exn)]
  ;;
end

module Property_name = struct
  module type S = sig
    module Property_value : sig
      type t [@@deriving sexp_of]
      val of_value_exn : Value.t -> t
      val to_value : t -> Value.t
    end
    val name : Symbol.t
  end

  type 'a t = (module S with type Property_value.t = 'a)

  let name (type a) (t : a t) =
    let module T = (val t) in
    T.name
  ;;

  let name_as_value t = t |> name |> Symbol.to_value

  let sexp_of_t _ t = [%sexp (name t : Symbol.t)]

  let of_value_exn (type a) (t : a t) =
    let module T = (val t) in
    T.Property_value.of_value_exn
  ;;

  let to_value (type a) (t : a t) =
    let module T = (val t) in
    T.Property_value.to_value
  ;;

  module Unknown = struct
    module Property_value = struct
      include Value
      let of_value_exn = Fn.id
      let to_value     = Fn.id
    end
  end

  module Packed = struct
    type 'a property_name = 'a t

    type t = T : _ property_name -> t

    let sexp_of_t (T p) = [%sexp (p : _ t)]

    let name (T p) = name p

    let name_as_value (T p) = name_as_value p

    let all_except_unknown = ref []

    let of_name_as_value_exn value =
      match Symbol.of_value_exn value with
      | exception _ ->
        raise_s [%message
          "[Text.Property.Packed.of_name_as_value_exn] got unexpected value"
            (value : Value.t)]
      | symbol ->
        match
          List.find !all_except_unknown ~f:(fun t -> Symbol.equal symbol (name t))
        with
        | Some t -> t
        | None ->
          T (module struct
            include Unknown
            let name = symbol
          end)
    ;;
  end

  let create_and_register (type a) (t : (module S with type Property_value.t = a)) =
    Packed.all_except_unknown := T t :: !Packed.all_except_unknown;
    t
  ;;

  module Face_name = struct
    module Property_value = Face_spec
  end

  let face : _ t =
    create_and_register
      (module struct
        include Face_name
        let name = Q.face
      end)
  ;;

  let font_lock_face : _ t =
    create_and_register
      (module struct
        include Face_name
        let name = Q.font_lock_face
      end)
  ;;
end

module Property = struct
  type t = T : 'a Property_name.t * 'a -> t

  let sexp_of_t (T (property_name, property_value)) =
    let module Property_name = (val property_name) in
    [%message
      ""
        ~_:(Property_name.name : Symbol.t)
        ~_:(property_value : Property_name.Property_value.t)]
  ;;

  let rec of_property_list_exn value =
    if Value.is_nil value
    then []
    else if not (Value.is_cons value)
    then raise_s [%message
           "[Text.Property.of_property_list_exn] got unexpected value" (value : Value.t)]
    else (
      let module N = Property_name.Packed in
      let N.T property_name = Value.car_exn value |> N.of_name_as_value_exn in
      let property_value_and_rest = Value.cdr_exn value in
      T (property_name
        , Value.car_exn property_value_and_rest
          |> Property_name.of_value_exn property_name)
      :: of_property_list_exn (Value.cdr_exn property_value_and_rest))
  ;;

  let to_property_list ts =
    List.fold (List.rev ts) ~init:[] ~f:(fun ac (T (name, value)) ->
      let module Name = (val name) in
      (Name.name |> Symbol.to_value)
      :: (value |> Name.Property_value.to_value)
      :: ac)
  ;;
end

let propertize t properties =
  Symbol.funcallN Q.propertize
    ((t |> to_value) :: (properties |> Property.to_property_list))
  |> of_value_exn
;;

let property_value t ~at property_name =
  let value =
    Symbol.funcall3 Q.get_text_property
      (at |> Value.of_int_exn)
      (property_name |> Property_name.name |> Symbol.to_value)
      (t |> to_value) in
  if Value.is_nil value
  then None
  else Some (value |> Property_name.of_value_exn property_name)
;;

let properties t ~at =
  Symbol.funcall2 Q.text_properties_at
    (at |> Value.of_int_exn)
    (t |> to_value)
  |> Property.of_property_list_exn
;;

let get_start start =
  (match start with
   | Some i -> i
   | None -> 0)
  |> Value.of_int_exn
;;

let get_end t end_ =
  (match end_ with
   | Some i -> i
   | None -> length t)
  |> Value.of_int_exn
;;

let set_property ?start ?end_ t property_name property_value =
  Symbol.funcall5_i Q.put_text_property
    (start |> get_start)
    (end_  |> get_end t)
    (property_name |> Property_name.name_as_value)
    (property_value |> Property_name.to_value property_name)
    (t |> to_value);
;;

let add_properties ?start ?end_ t properties =
  Symbol.funcall4_i Q.add_text_properties
    (start |> get_start)
    (end_  |> get_end t)
    (properties |> Property.to_property_list |> Value.list)
    (t |> to_value);
;;

let set_properties ?start ?end_ t properties =
  Symbol.funcall4_i Q.set_text_properties
    (start |> get_start)
    (end_  |> get_end t)
    (properties |> Property.to_property_list |> Value.list)
    (t |> to_value);
;;

let remove_properties ?start ?end_ t property_names =
  Symbol.funcall4_i Q.remove_list_of_text_properties
    (start |> get_start)
    (end_  |> get_end t)
    (property_names |> List.map ~f:Property_name.Packed.name_as_value |> Value.list)
    (t |> to_value);
;;

let is_multibyte t =
  Symbol.funcall1 Q.multibyte_string_p (t |> to_value) |> Value.to_bool
;;

let num_bytes t =
  Symbol.funcall1 Q.string_bytes (t |> to_value) |> Value.to_int_exn
;;

let to_multibyte t =
  Symbol.funcall1 Q.string_to_multibyte (t |> to_value) |> of_value_exn
;;

let to_unibyte_exn t =
  Symbol.funcall1 Q.string_to_unibyte (t |> to_value) |> of_value_exn
;;
