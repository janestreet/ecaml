open! Core_kernel
open! Import

module Q = struct
  include Q
  let tabulated_list_entries           = "tabulated-list-entries"           |> Symbol.intern
  let tabulated_list_format            = "tabulated-list-format"            |> Symbol.intern
  let tabulated_list_get_id            = "tabulated-list-get-id"            |> Symbol.intern
  let tabulated_list_init_header       = "tabulated-list-init-header"       |> Symbol.intern
  let tabulated_list_mode              = "tabulated-list-mode"              |> Symbol.intern
  let tabulated_list_print             = "tabulated-list-print"             |> Symbol.intern
  let tabulated_list_sort_key          = "tabulated-list-sort-key"          |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let tabulated_list_get_id =
    Q.tabulated_list_get_id <: nullary @-> return (option value)
  let tabulated_list_init_header =
    Q.tabulated_list_init_header <: nullary @-> return nil
  let tabulated_list_print =
    Q.tabulated_list_print <: bool @-> bool @-> return nil
end

module Column = struct
  module Format = struct
    type t =
      { align_right : bool
      ; header      : string
      ; pad_right   : int
      ; sortable    : bool
      ; width       : int }
    [@@deriving sexp_of]

    let create ?(align_right=false) ?(pad_right=1) ?(sortable=true) ~header ~width () =
      { align_right; pad_right; sortable; header; width }
    ;;

    module Property = struct
      type t =
        | Align_right of bool
        | Pad_right of int

      let to_values = function
        | Align_right b ->
          [ Q.K.right_align |> Symbol.to_value
          ; b               |> Value.of_bool ]
        | Pad_right padding ->
          [ Q.K.pad_right |> Symbol.to_value
          ; padding       |> Value.of_int_exn ]
      ;;

      let of_values (keyword, value) =
        if Value.equal (Q.K.right_align |> Symbol.to_value) keyword
        then Align_right (Value.to_bool value)
        else if Value.equal (Q.K.pad_right |> Symbol.to_value) keyword
        then Pad_right (Value.to_int_exn value)
        else raise_s [%sexp "Invalid Property keyword", (keyword : Value.t), (value : Value.t)]
      ;;
    end

    module Properties = struct

      let rec pairs = function
        | [] -> []
        | [ _ ] -> raise_s [%sexp "Received an odd number of list elements"]
        | a :: b :: rest -> (a,b) :: pairs rest
      ;;

      let type_ =
        let open Value.Type in
        map (list value) ~name:[%sexp "tabulated-list-column-format-property-list"]
          ~of_:(fun x -> List.map (pairs x) ~f:Property.of_values)
          ~to_:(List.concat_map   ~f:Property.to_values)
      ;;
    end

    let type_ =
      let open Value.Type in
      map (tuple string
             (tuple int
                (tuple bool Properties.type_)))
        ~name:[%sexp "Column.Format"]
        ~of_:(fun (header, (width, (sortable, props))) ->
          let t = create ~header ~sortable ~width () in
          List.fold props ~init:t ~f:(fun t -> function
            | Align_right align_right -> { t with align_right }
            | Pad_right   pad_right   -> { t with pad_right   }))
        ~to_:(fun {align_right; header; pad_right; sortable; width} ->
          (header, (width, (sortable, [ Align_right align_right
                                      ; Pad_right pad_right]))))
    ;;
  end

  type 'record t =
    { field_of_record : 'record  -> string
    ; format          : Format.t }
  [@@deriving fields]

  let create ?align_right ?pad_right ?sortable ~header ~width field_of_record =
    { field_of_record
    ; format = Format.create ?align_right ?pad_right ?sortable ~header ~width () }
  ;;

end

type ('record, 'id) t =
  { columns      : 'record Column.t list
  ; entries_var  : 'record list Var.t
  ; id_of_record : 'record -> 'id
  ; id_type      : 'id Value.Type.t
  ; major_mode   : Major_mode.t }
[@@deriving fields]

let tabulated_list_format_var =
  Var.create Q.tabulated_list_format (Value.Type.vector Column.Format.type_)
;;

let tabulated_list_sort_key_var =
  Var.create Q.tabulated_list_sort_key Value.Type.(option (tuple string bool))
;;

let draw ?sort_by t rows =
  (* Work around an emacs bug where tabulated-list.el doesn't check that we're sorting by
     a sortable column *)
  Option.iter sort_by ~f:(fun (sort_header, _) ->
    match List.find t.columns ~f:(fun column ->
      String.equal sort_header column.format.header)
    with
    | None -> raise_s [%sexp "Unknown header to sort by", (sort_header : string)];
    | Some column ->
      if not column.format.sortable
      then raise_s [%sexp "Column is not sortable", (sort_header : string)]);
  Current_buffer.set_value tabulated_list_sort_key_var
    (Option.map sort_by ~f:(Tuple2.map_snd ~f:(function
       | `Ascending  -> false
       | `Descending -> true)));
  Current_buffer.set_value t.entries_var rows;
  F.tabulated_list_print false false;
;;

let tabulated_list_mode = Major_mode.create ~change_command:Q.tabulated_list_mode

let create here columns ~docstring ~id_type ~id_of_record ~initialize ~mode_change_command
      ~mode_line =
  let major_mode =
    Major_mode.define_derived_mode here
      ~change_command:mode_change_command
      ~docstring
      ~parent:tabulated_list_mode
      ~mode_line
      ~initialize:(fun () ->
        Current_buffer.set_value tabulated_list_format_var
          (Array.of_list (List.map columns ~f:Column.format));
        F.tabulated_list_init_header ();
        initialize ())
  in
  let entries_var =
    let entry_type =
      let open Value.Type in
      map (tuple id_type (tuple (vector string) unit))
        ~name:[%message "tabulated-list-entries" (id_type.name : Sexp.t)]
        ~of_:(fun _ -> raise_s [%sexp "reading tabulated-list-entries is not supported"])
        ~to_:(fun record ->
          (id_of_record record,
           (columns
            |> List.map ~f:(fun (column : _ Column.t) -> column.field_of_record record)
            |> Array.of_list,
            ())))
    in
    Var.create Q.tabulated_list_entries (Value.Type.list entry_type)
  in
  { columns
  ; entries_var
  ; id_of_record
  ; id_type
  ; major_mode }
;;

let get_id_at_point_exn t =
  Option.map (F.tabulated_list_get_id ()) ~f:t.id_type.of_value_exn
