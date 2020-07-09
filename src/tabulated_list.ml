open! Core_kernel
open! Import

module Column = struct
  module Format = struct
    type 'a t =
      { align_right : bool
      ; header : string
      ; pad_right : int
      ; sortable : bool
      ; width : 'a
      }
    [@@deriving sexp_of]

    let create
          ?(align_right = false)
          ?(pad_right = 1)
          ?(sortable = true)
          ~header
          ~width
          ()
      =
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
          (tuple string (tuple int (tuple bool Properties.t)))
          ~name:[%sexp "Column.Format"]
          ~of_:(fun (header, (width, (sortable, props))) ->
            let t = format ~header ~sortable ~width () in
            List.fold props ~init:t ~f:(fun t ->
              function
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

  type 'record t =
    { field_of_record : 'record -> Text.t
    ; format : Variable_width.t Format.t
    }
  [@@deriving fields]

  let text
        ?align_right
        ?max_width
        ?min_width
        ?pad_right
        ?sortable
        ~header
        field_of_record
    =
    { field_of_record
    ; format =
        (let min_width = Option.fold min_width ~init:(String.length header) ~f:Int.max in
         let width = Variable_width.create ~max_width ~min_width in
         Format.create ?align_right ?pad_right ?sortable ~header ~width ())
    }
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

  let fixed_width_format t values =
    { t.format with
      width =
        List.map values ~f:t.field_of_record
        |> Variable_width.to_fixed_width t.format.width
    }
  ;;

  let first_line
        ?align_right
        ?max_width
        ?min_width
        ?pad_right
        ?sortable
        ~header
        field_of_record
    =
    create ?align_right ?max_width ?min_width ?pad_right ?sortable ~header (fun record ->
      let str = field_of_record record in
      (match String.split_lines str with
       | line :: _ :: _ -> sprintf "%s..." (String.rstrip line)
       | [] | [ _ ] -> str)
      |> String.strip)
  ;;
end

type ('record, 'id) t =
  { columns : 'record Column.t list
  ; entries_var : 'record list Buffer_local.t
  ; id_equal : 'id -> 'id -> bool
  ; id_of_record : 'record -> 'id
  ; id_type : 'id Value.Type.t
  ; major_mode : Major_mode.t
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
  (* Work around an emacs bug where tabulated-list.el doesn't check that we're sorting by
     a sortable column *)
  Option.iter sort_by ~f:(fun (sort_header, _) ->
    match
      List.find t.columns ~f:(fun column ->
        String.equal sort_header column.format.header)
    with
    | None -> raise_s [%sexp "Unknown header to sort by", (sort_header : string)]
    | Some column ->
      if not column.format.sortable
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

let create major_mode columns ~id_equal ~id_type ~id_of_record =
  if not (Major_mode.is_derived major_mode ~from:Tabulated_list_mode.major_mode)
  then
    raise_s
      [%sexp
        "[Tabulated_list.create] called on a major mode not derived from \
         [Tabulated_list.Tabulated_list_mode.major_mode]."];
  let entries_var =
    let entry_type =
      let open Value.Type in
      map
        (tuple id_type (tuple (vector Text.t) unit))
        ~name:[%message "tabulated-list-entries" (id_type : _ Value.Type.t)]
        ~of_:(fun _ -> raise_s [%sexp "reading tabulated-list-entries is not supported"])
        ~to_:(fun record ->
          ( id_of_record record
          , ( columns
              |> List.map ~f:(fun (column : _ Column.t) -> column.field_of_record record)
              |> Array.of_list
            , () ) ))
    in
    Buffer_local.Wrap.("tabulated-list-entries" <: list entry_type)
  in
  { columns; entries_var; id_equal; id_of_record; id_type; major_mode }
;;

let tabulated_list_get_id =
  Funcall.Wrap.("tabulated-list-get-id" <: nullary @-> return (nil_or value))
;;

let get_id_at_point_exn t =
  Option.map (tabulated_list_get_id ()) ~f:(Value.Type.of_value_exn t.id_type)
;;

let move_point_to_id t id =
  Point.goto_min ();
  let rec loop () =
    match get_id_at_point_exn t with
    | None ->
      (* This only happens after we go past the last row. *)
      raise_s [%sexp "Could not find row with given id"]
    | Some id_at_point ->
      (match t.id_equal id id_at_point with
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

let revert_hook =
  Hook.create ("tabulated-list-revert-hook" |> Symbol.intern) ~hook_type:Normal
;;
