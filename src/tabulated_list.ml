open! Core
open! Import

module Column = struct
  module Sortable = struct
    type t =
      | Not_sortable
      | Emacs_text_sort
      | Use_function of Function.t
    [@@deriving sexp_of]

    let is_sortable = function
      | Not_sortable -> false
      | Emacs_text_sort | Use_function _ -> true
    ;;

    let create ~get_field ~sortable ~entry_type =
      match sortable with
      | `Not_sortable -> Not_sortable
      | `Emacs_text_sort -> Emacs_text_sort
      | `Use_function compare ->
        Defun.lambda
          [%here]
          (Returns Value.Type.bool)
          (let%map_open.Defun a = required "a" entry_type
           and b = required "b" entry_type in
           compare (get_field a) (get_field b) < 0)
        |> Use_function
    ;;

    let to_value = function
      | Not_sortable -> Value.nil
      | Emacs_text_sort -> Value.t
      | Use_function f -> Function.to_value f
    ;;

    let of_value v =
      if Value.is_nil v
      then Not_sortable
      else if Value.equal v Value.t
      then Emacs_text_sort
      else Use_function (Function.of_value_exn v)
    ;;

    let t = Value.Type.create [%sexp "Column.Sortable"] sexp_of_t of_value to_value
  end

  module Format = struct
    type 'a t =
      { align_right : bool
      ; header : string
      ; pad_right : int
      ; sortable : Sortable.t
      ; width : 'a
      }
    [@@deriving sexp_of]

    let create ?(align_right = false) ?(pad_right = 1) ~sortable ~header ~width () =
      { align_right; pad_right; sortable; header; width }
    ;;

    module Fixed_width = struct
      type nonrec t = int t [@@deriving sexp_of]

      module Property = struct
        type t =
          | Align_right of bool
          | Pad_right of int

        let to_values = function
          | Align_right b -> [ Q.K.right_align |> Symbol.to_value; b |> Value.of_bool ]
          | Pad_right padding ->
            [ Q.K.pad_right |> Symbol.to_value; padding |> Value.of_int_exn ]
        ;;

        let of_values (keyword, value) =
          if Value.equal (Q.K.right_align |> Symbol.to_value) keyword
          then Align_right (Value.to_bool value)
          else if Value.equal (Q.K.pad_right |> Symbol.to_value) keyword
          then Pad_right (Value.to_int_exn value)
          else
            raise_s
              [%sexp "Invalid Property keyword", (keyword : Value.t), (value : Value.t)]
        ;;
      end

      module Properties = struct
        let rec pairs = function
          | [] -> []
          | [ _ ] -> raise_s [%sexp "Received an odd number of list elements"]
          | a :: b :: rest -> (a, b) :: pairs rest
        ;;

        let type_ =
          let open Value.Type in
          map
            (list value)
            ~name:[%sexp "tabulated-list-column-format-property-list"]
            ~of_:(fun x -> List.map (pairs x) ~f:Property.of_values)
            ~to_:(List.concat_map ~f:Property.to_values)
        ;;

        let t = type_
      end

      let type_ =
        let format = create in
        let open Value.Type in
        map
          (tuple string (tuple int (tuple Sortable.t Properties.t)))
          ~name:[%sexp "Column.Format"]
          ~of_:(fun (header, (width, (sortable, props))) ->
            let t = format ~header ~sortable ~width () in
            List.fold props ~init:t ~f:(fun t -> function
              | Align_right align_right -> { t with align_right }
              | Pad_right pad_right -> { t with pad_right }))
          ~to_:(fun { align_right; header; pad_right; sortable; width } ->
            header, (width, (sortable, [ Align_right align_right; Pad_right pad_right ])))
      ;;

      let t = type_
    end
  end

  module Variable_width = struct
    type t =
      { max_width : int option
      ; min_width : int
      }
    [@@deriving compare, fields, sexp_of]

    let create = Fields.create

    let to_fixed_width t values =
      let width =
        List.map values ~f:Text.length
        |> List.max_elt ~compare:[%compare: int]
        |> Option.value ~default:0
      in
      Option.fold t.max_width ~init:width ~f:Int.min |> Int.max t.min_width
    ;;
  end

  module Spec = struct
    type ('record, 'field) t_unpacked =
      { get_field : 'record -> 'field
      ; render_field : 'field -> Text.t
      ; sortable :
          [ `Not_sortable | `Emacs_text_sort | `Use_function of 'field -> 'field -> int ]
      ; align_right : bool option
      ; max_width : int option
      ; min_width : int option
      ; pad_right : int option
      ; header : string
      }
    [@@deriving fields]

    type 'record t = T : ('record, 'field) t_unpacked -> 'record t

    let render_field (T t) record = record |> t.get_field |> t.render_field

    let create
          ~get_field
          ~render_field
          ~sortable
          ?align_right
          ?max_width
          ?min_width
          ?pad_right
          ~header
          ()
      =
      Fields_of_t_unpacked.create
        ~get_field
        ~render_field
        ~sortable
        ~align_right
        ~max_width
        ~min_width
        ~pad_right
        ~header
      |> T
    ;;
  end

  type 'record t_internal =
    { spec : 'record Spec.t
    ; format : Variable_width.t Format.t
    }
  [@@deriving fields]

  let is_sortable t = Sortable.is_sortable t.format.sortable
  let render_field t v = Spec.render_field t.spec v

  let create_internal
        ((T
            { get_field
            ; render_field = _
            ; sortable
            ; align_right
            ; max_width
            ; min_width
            ; pad_right
            ; header
            } :
            _ Spec.t) as spec)
        entry_type
    =
    { spec
    ; format =
        (let min_width = Option.fold min_width ~init:(String.length header) ~f:Int.max in
         let width = Variable_width.create ~max_width ~min_width in
         let sortable = Sortable.create ~get_field ~sortable ~entry_type in
         Format.create ?align_right ?pad_right ~sortable ~header ~width ())
    }
  ;;

  let fixed_width_format t values =
    { t.format with
      width =
        List.map values ~f:(render_field t)
        |> Variable_width.to_fixed_width t.format.width
    }
  ;;

  (* expose Spec.t as t here to have a nice interface outside *)
  type 'record t = 'record Spec.t

  let create_gen
        ?align_right
        ?max_width
        ?min_width
        ?pad_right
        ~header
        ~get_field
        ~render_field
        ~compare_field
        ()
    =
    Spec.create
      ~get_field
      ~render_field
      ~sortable:(`Use_function compare_field)
      ?align_right
      ?max_width
      ?min_width
      ?pad_right
      ~header
      ()
  ;;

  let text
        ?align_right
        ?max_width
        ?min_width
        ?pad_right
        ?(sortable = true)
        ~header
        field_of_record
    =
    Spec.create
      ~get_field:field_of_record
      ~render_field:Fn.id
      ~header
      ?align_right
      ?max_width
      ?min_width
      ?pad_right
      ~sortable:(if sortable then `Emacs_text_sort else `Not_sortable)
      ()
  ;;

  let create
        ?align_right
        ?max_width
        ?min_width
        ?pad_right
        ?sortable
        ~header
        field_of_record
    =
    text
      ?align_right
      ?max_width
      ?min_width
      ?pad_right
      ?sortable
      ~header
      (field_of_record >> Text.of_utf8_bytes)
  ;;
end

type 'record t =
  { columns : 'record Column.t_internal list
  ; entries_var : 'record list Buffer_local.t
  ; major_mode : Major_mode.t
  ; type_ : 'record Value.Type.t
  }
[@@deriving fields]

let keymap t = Major_mode.keymap (major_mode t)

let tabulated_list_format_var =
  Buffer_local.Wrap.("tabulated-list-format" <: vector Column.Format.Fixed_width.t)
;;

let tabulated_list_sort_key_var =
  Buffer_local.Wrap.("tabulated-list-sort-key" <: nil_or (tuple string bool))
;;

let tabulated_list_init_header =
  Funcall.Wrap.("tabulated-list-init-header" <: nullary @-> return nil)
;;

let tabulated_list_print =
  Funcall.Wrap.("tabulated-list-print" <: bool @-> bool @-> return nil)
;;

let draw ?sort_by t rows =
  (* tabulated-list.el doesn't check that we're sorting by a sortable column, if we just
     set [tabulated-list-sort-key].  Instead, it just displays the list unsorted.  We
     prefer to raise instead.

     It only signals an error if you invoke [tabulated-list-sort] with point in an
     unsortable column. *)
  Option.iter sort_by ~f:(fun (sort_header, _) ->
    match
      List.find t.columns ~f:(fun column -> String.equal sort_header column.format.header)
    with
    | None -> raise_s [%sexp "Unknown header to sort by", (sort_header : string)]
    | Some column ->
      if not (Column.is_sortable column)
      then raise_s [%sexp "Column is not sortable", (sort_header : string)]);
  Current_buffer.set_buffer_local
    tabulated_list_sort_key_var
    (Option.map
       sort_by
       ~f:
         (Tuple2.map_snd ~f:(function
            | `Ascending -> false
            | `Descending -> true)));
  Current_buffer.set_buffer_local t.entries_var rows;
  Current_buffer.set_buffer_local
    tabulated_list_format_var
    (Array.of_list
       (List.map t.columns ~f:(fun column -> Column.fixed_width_format column rows)));
  tabulated_list_init_header ();
  tabulated_list_print true false
;;

module Tabulated_list_mode = (val Major_mode.wrap_existing "tabulated-list-mode" [%here])

let create major_mode (type a) (column_specs : a Column.t list) =
  if not (Major_mode.is_derived major_mode ~from:Tabulated_list_mode.major_mode)
  then
    raise_s
      [%sexp
        "[Tabulated_list.create] called on a major mode not derived from \
         [Tabulated_list.Tabulated_list_mode.major_mode]."];
  let type_ : a Value.Type.t =
    Type_equal.Id.create ~name:(Current_buffer.name ()) [%sexp_of: _]
    |> Caml_embed.create_type
  in
  let entry_type =
    let open Value.Type in
    map
      (tuple type_ (tuple (vector Text.t) unit))
      ~name:[%message "tabulated-list-entries" ~_:(type_ : _ Value.Type.t)]
      ~of_:fst
      ~to_:(fun record ->
        ( record
        , ( List.map column_specs ~f:(fun spec -> Column.Spec.render_field spec record)
            |> Array.of_list
          , () ) ))
  in
  let columns =
    List.map column_specs ~f:(fun spec -> Column.create_internal spec entry_type)
  in
  let entries_var = Buffer_local.Wrap.("tabulated-list-entries" <: list entry_type) in
  { columns; entries_var; major_mode; type_ }
;;

let tabulated_list_get_id =
  Funcall.Wrap.("tabulated-list-get-id" <: nullary @-> return (nil_or value))
;;

let get_record_at_point_exn t =
  Option.map (tabulated_list_get_id ()) ~f:(Value.Type.of_value_exn t.type_)
;;

let move_point_to_record t ~f =
  Point.goto_min ();
  let rec loop () =
    match get_record_at_point_exn t with
    | None ->
      (* This only happens after we go past the last row. *)
      raise_s [%sexp "Could not find row with given id"]
    | Some record ->
      (match f record with
       | false ->
         Point.forward_line 1;
         loop ()
       | true -> ())
  in
  loop ()
;;

let current_buffer_has_entries () =
  not
    (Value.is_nil
       (Current_buffer.get_buffer_local
          Buffer_local.Wrap.("tabulated-list-entries" <: value)))
;;

let revert_hook = Hook.Wrap.("tabulated-list-revert-hook" <: Normal_hook)
