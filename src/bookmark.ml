open! Core_kernel
open! Import

let () = Feature.require ("bookmark" |> Symbol.intern)

include (
  String :
  sig
    type t = string [@@deriving sexp_of]

    include Comparator.S with type t := t
    include Stringable.S with type t := t
  end)

include (val Valueable.of_type Value.Type.string)

module Property = struct
  type 'a t =
    { symbol : Symbol.t
    ; type_ : 'a Value.Type.t
    }
  [@@deriving sexp_of]

  module And_value = struct
    type 'a property = 'a t
    type t = T : 'a property * 'a -> t
  end

  let and_value t a = And_value.T (t, a)
  let create symbol type_ = { symbol = Symbol.intern symbol; type_ }
  let annotation = create "annotation" Value.Type.string
  let filename = create "filename" Value.Type.string
  let front_context_string = create "front-context-string" Value.Type.string
  let handler = create "handler" Function.type_
  let position = create "position" Position.type_
  let rear_context_string = create "rear-context-string" Value.Type.string
end

module Record = struct
  type t = Value.t Map.M(Symbol.Compare_name).t [@@deriving sexp_of]

  include Valueable.Remove_t
      ((val Valueable.of_type
              (Value.Type.(map (list (tuple Symbol.type_ value)))
                 ~name:[%message "bookmark-record"]
                 ~of_:(Map.of_alist_exn (module Symbol.Compare_name))
                 ~to_:Map.to_alist)))

  let get t { Property.symbol; type_ } =
    Map.find t symbol |> Option.map ~f:(Value.Type.of_value_exn type_)
  ;;

  let get_exn t property =
    match get t property with
    | None ->
      raise_s
        [%message
          "Bookmark record missing property." ~_:(property.symbol : Symbol.t) (t : t)]
    | Some value -> value
  ;;

  let set t { Property.symbol; type_ } data =
    Map.set t ~key:symbol ~data:(Value.Type.to_value type_ data)
  ;;

  let create =
    List.map ~f:(fun (Property.And_value.T (property, value)) ->
      property.symbol, Value.Type.to_value property.type_ value)
    >> Map.of_alist_exn (module Symbol.Compare_name)
  ;;
end

let bookmark_store =
  Funcall.Wrap.("bookmark-store" <: t @-> Record.t @-> bool @-> return nil)
;;

let set t bookmark_record ~no_overwrite = bookmark_store t bookmark_record no_overwrite

let bookmark_get_bookmark_record =
  Funcall.Wrap.("bookmark-get-bookmark-record" <: value @-> return Record.t)
;;

let param of_record_exn =
  let%map_open.Defun bookmark = required "bookmark" value in
  bookmark_get_bookmark_record bookmark |> of_record_exn
;;

module Make_record_function = struct
  module Return_type = struct
    type t =
      { record : Record.t
      ; suggested_bookmark_name : string option
      }

    include Valueable.Remove_t
        ((val Valueable.of_type
                (Value.Type.(map (tuple (nil_or string) Record.t))
                   ~name:[%sexp "bookmark-make-record-function-return-type"]
                   ~of_:(fun (suggested_bookmark_name, record) ->
                     { record; suggested_bookmark_name })
                   ~to_:(fun { record; suggested_bookmark_name } ->
                     suggested_bookmark_name, record))))
  end

  type t = unit -> Return_type.t

  let in_buffer =
    Buffer_local.wrap_existing
      (Symbol.intern "bookmark-make-record-function")
      Symbol.t
      (* This might break code which expects to alter the global value. But this variable
         only makes sense as a buffer-local value. People who want to change the default
         make record behavior would be better off advising the function
         [bookmark-make-record-function-default]. Let's try this until we find a problem.
      *)
      ~make_buffer_local_always:true
  ;;

  let bookmark_make_record_default =
    Funcall.Wrap.(
      "bookmark-make-record-default"
      <: nil_or bool @-> nil_or bool @-> nil_or Position.t @-> return Return_type.t)
  ;;

  let default ?no_context ?no_file ?position () =
    bookmark_make_record_default no_file no_context position
  ;;
end
