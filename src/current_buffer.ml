open! Core_kernel
open! Import

module Q = struct
  include Current_buffer0.Q
  let buffer_disable_undo                 = "buffer-disable-undo"                 |> Symbol.intern
  let buffer_enable_undo                  = "buffer-enable-undo"                  |> Symbol.intern
  let buffer_modified_p                   = "buffer-modified-p"                   |> Symbol.intern
  let buffer_restore_window_display_state = "Buffer.restore-window-display-state" |> Symbol.intern
  let buffer_substring                    = "buffer-substring"                    |> Symbol.intern
  let buffer_substring_no_properties      = "buffer-substring-no-properties"      |> Symbol.intern
  let buffer_undo_list                    = "buffer-undo-list"                    |> Symbol.intern
  let buffer_window_display_state         = "Buffer.window-display-state"         |> Symbol.intern
  let bury_buffer                         = "bury-buffer"                         |> Symbol.intern
  let char_syntax                         = "char-syntax"                         |> Symbol.intern
  let current_local_map                   = "current-local-map"                   |> Symbol.intern
  let current_minor_mode_maps             = "current-minor-mode-maps"             |> Symbol.intern
  let deactivate_mark                     = "deactivate-mark"                     |> Symbol.intern
  let default_directory                   = "default-directory"                   |> Symbol.intern
  let delete_duplicate_lines              = "delete-duplicate-lines"              |> Symbol.intern
  let delete_region                       = "delete-region"                       |> Symbol.intern
  let enable_multibyte_characters         = "enable-multibyte-characters"         |> Symbol.intern
  let erase_buffer                        = "erase-buffer"                        |> Symbol.intern
  let fill_column                         = "fill-column"                         |> Symbol.intern
  let flush_lines                         = "flush-lines"                         |> Symbol.intern
  let ignore_auto                         = "ignore-auto"                         |> Symbol.intern
  let indent_region                       = "indent-region"                       |> Symbol.intern
  let kill_local_variable                 = "kill-local-variable"                 |> Symbol.intern
  let kill_region                         = "kill-region"                         |> Symbol.intern
  let local_variable_if_set_p             = "local-variable-if-set-p"             |> Symbol.intern
  let local_variable_p                    = "local-variable-p"                    |> Symbol.intern
  let major_mode                          = "major-mode"                          |> Symbol.intern
  let make_local_variable                 = "make-local-variable"                 |> Symbol.intern
  let mark_active                         = "mark-active"                         |> Symbol.intern
  let mark_marker                         = "mark-marker"                         |> Symbol.intern
  let noconfirm                           = "noconfirm"                           |> Symbol.intern
  let paragraph_separate                  = "paragraph-separate"                  |> Symbol.intern
  let paragraph_start                     = "paragraph-start"                     |> Symbol.intern
  let rename_buffer                       = "rename-buffer"                       |> Symbol.intern
  let revert_buffer                       = "revert-buffer"                       |> Symbol.intern
  let revert_buffer_function              = "revert-buffer-function"              |> Symbol.intern
  let save_buffer                         = "save-buffer"                         |> Symbol.intern
  let set_buffer_modified_p               = "set-buffer-modified-p"               |> Symbol.intern
  let set_buffer_multibyte                = "set-buffer-multibyte"                |> Symbol.intern
  let set_mark                            = "set-mark"                            |> Symbol.intern
  let set_syntax_table                    = "set-syntax-table"                    |> Symbol.intern
  let sort_lines                          = "sort-lines"                          |> Symbol.intern
  let syntax_table                        = "syntax-table"                        |> Symbol.intern
  let text_property_not_all               = "text-property-not-all"               |> Symbol.intern
  let transient_mark_mode                 = "transient-mark-mode"                 |> Symbol.intern
  let undo                                = "undo"                                |> Symbol.intern
  let undo_boundary                       = "undo-boundary"                       |> Symbol.intern
  let use_local_map                       = "use-local-map"                       |> Symbol.intern
  let widen                               = "widen"                               |> Symbol.intern
end

include (Current_buffer0 : Current_buffer0_intf.Current_buffer0 with module Q := Q)

module Window_display_state = struct
  include Value.Make_subtype
      (struct
        let here = [%here]
        let name = "Buffer.window-display-state"
        let is_in_subtype =
          let open Value in
          is_cons ~car:Position.is_in_subtype
            ~cdr:(is_cons ~car:Position.is_in_subtype ~cdr:Position.is_in_subtype)
        ;;
      end)
end

module F = struct
  open Value.Type
  open Funcall

  let buffer_disable_undo = Q.buffer_disable_undo <: nullary @-> return nil
  let buffer_enable_undo  = Q.buffer_enable_undo  <: nullary @-> return nil
  let bury_buffer         = Q.bury_buffer         <: nullary @-> return nil
  let deactivate_mark     = Q.deactivate_mark     <: nullary @-> return nil
  let erase_buffer        = Q.erase_buffer        <: nullary @-> return nil
  let kill_buffer         = Q.kill_buffer         <: nullary @-> return nil
  let save_buffer         = Q.save_buffer         <: nullary @-> return nil
  let undo_boundary       = Q.undo_boundary       <: nullary @-> return nil
  let widen               = Q.widen               <: nullary @-> return nil

  let buffer_local_variables  = Q.buffer_local_variables  <: nullary @-> return (list value)
  let buffer_modified_p       = Q.buffer_modified_p       <: nullary @-> return bool
  let current_local_map       = Q.current_local_map       <: nullary @-> return (option Keymap.type_)
  let current_minor_mode_maps = Q.current_minor_mode_maps <: nullary @-> return (list Keymap.type_)
  let mark_marker             = Q.mark_marker             <: nullary @-> return Marker.type_
  let syntax_table            = Q.syntax_table            <: nullary @-> return Syntax_table.type_

  let delete_region         = Q.delete_region         <: Position.type_ @-> Position.type_ @-> return nil
  let indent_region         = Q.indent_region         <: Position.type_ @-> Position.type_ @-> return nil
  let kill_local_variable   = Q.kill_local_variable   <: Symbol.type_                      @-> return nil
  let kill_region           = Q.kill_region           <: Position.type_ @-> Position.type_ @-> return nil
  let make_local_variable   = Q.make_local_variable   <: Symbol.type_                      @-> return nil
  let rename_buffer         = Q.rename_buffer         <: string @-> bool                   @-> return nil
  let revert_buffer         = Q.revert_buffer         <: value @-> bool                    @-> return nil
  let set_buffer_modified_p = Q.set_buffer_modified_p <: bool                              @-> return nil
  let set_buffer_multibyte  = Q.set_buffer_multibyte  <: bool                              @-> return nil
  let use_local_map         = Q.use_local_map         <: Keymap.type_                      @-> return nil
  let set_mark              = Q.set_mark              <: Position.type_                    @-> return nil
  let set_marker            = Q.set_marker            <: Marker.type_ @-> Position.type_   @-> return nil
  let set_syntax_table      = Q.set_syntax_table      <: Syntax_table.type_                @-> return nil
  let undo                  = Q.undo                  <: int                               @-> return nil

  let char_syntax             = Q.char_syntax             <: Char_code.type_ @-> return Char_code.type_
  let local_variable_p        = Q.local_variable_p        <: Symbol.type_    @-> return bool
  let local_variable_if_set_p = Q.local_variable_if_set_p <: Symbol.type_    @-> return bool
  let buffer_window_display_state =
    Q.buffer_window_display_state <: nullary @-> return Window_display_state.type_
  let buffer_restore_window_display_state =
    Q.buffer_restore_window_display_state <: Window_display_state.type_ @-> return nil

  let add_text_properties =
    Q.add_text_properties <: Position.type_ @-> Position.type_ @-> list value @-> return nil

  let buffer_substring =
    Q.buffer_substring <: Position.type_ @-> Position.type_ @-> return Text.type_

  let buffer_substring_no_properties =
    Q.buffer_substring_no_properties
    <: Position.type_ @-> Position.type_ @-> return Text.type_

  let delete_duplicate_lines =
    Q.delete_duplicate_lines <: Position.type_ @-> Position.type_ @-> return nil

  let flush_lines =
    Q.flush_lines <: Regexp.type_ @-> Position.type_ @-> Position.type_ @-> return nil

  let put_text_property =
    Q.put_text_property
    <: Position.type_ @-> Position.type_ @-> Symbol.type_ @-> value @-> return nil

  let set_text_properties =
    Q.set_text_properties <: Position.type_ @-> Position.type_ @-> list value @-> return nil

  let sort_lines =
    Q.sort_lines <: value @-> Position.type_ @-> Position.type_ @-> return nil

  let text_property_not_all =
    Q.text_property_not_all
    <: Position.type_ @-> Position.type_ @-> Symbol.type_ @-> value @-> return value

end

let set_temporarily t ~f =
  let old = get () in
  set t;
  protect ~f ~finally:(fun () -> set old)
;;

let set_temporarily_to_temp_buffer f =
  let t = Buffer.create ~name:"*temp-buffer*" in
  protect ~f:(fun () -> set_temporarily t ~f)
    ~finally:(fun () -> Buffer.kill t)
;;

let major_mode () =
  Major_mode.create
    ~change_command:(value_exn (Var.create Q.major_mode Symbol.type_))
;;

let change_major_mode major_mode =
  Funcall.(Major_mode.change_command major_mode <: nullary @-> return nil) ()
;;

let bury = F.bury_buffer

let directory = Var.create Q.default_directory Value.Type.string

let is_modified = F.buffer_modified_p

let set_modified = F.set_buffer_modified_p

let fill_column = Var.create Q.fill_column Value.Type.int

let paragraph_start = Var.create Q.paragraph_start Regexp.type_

let paragraph_separate = Var.create Q.paragraph_separate Regexp.type_

let read_only = Var.create Q.buffer_read_only Value.Type.bool

let file_name () = Buffer.file_name (get ())

let file_name_exn () =
  match file_name () with
  | Some x -> x
  | None -> raise_s [%message "buffer does not have a file name" ~_:(get () : Buffer.t)]
;;

let name () =
  match Buffer.name (get ()) with
  | Some x -> x
  | None -> raise_s [%message "current buffer has nil buffer-name"]
;;

let file_name_var = Var.create Q.buffer_file_name Value.Type.string

let transient_mark_mode = Var.create Q.transient_mark_mode Value.Type.bool

let buffer_undo_list = Var.create Q.buffer_undo_list Value.Type.value

let is_undo_enabled () = not (Value.eq (value_exn buffer_undo_list) Value.t)

let set_undo_enabled bool =
  if bool
  then F.buffer_enable_undo ()
  else F.buffer_disable_undo ()
;;

let undo_list () = value_exn buffer_undo_list

let undo = F.undo

let add_undo_boundary = F.undo_boundary

let or_point_max option =
  match option with
  | Some x -> x
  | None -> Point.max ()
;;

let or_point_min option =
  match option with
  | Some x -> x
  | None -> Point.min ()
;;

let contents ?start ?end_ ?(text_properties = false) () =
  (if text_properties
   then F.buffer_substring
   else F.buffer_substring_no_properties)
    (or_point_min start)
    (or_point_max end_)
;;

let kill  = F.kill_buffer
let save  = F.save_buffer
let erase = F.erase_buffer

let delete_region ~start ~end_ = F.delete_region start end_
let kill_region   ~start ~end_ = F.kill_region   start end_

let widen = F.widen

let save_current_buffer     f = Save_wrappers.save_current_buffer     f
let save_excursion          f = Save_wrappers.save_excursion          f
let save_mark_and_excursion f = Save_wrappers.save_mark_and_excursion f
let save_restriction        f = Save_wrappers.save_restriction        f

let save_window_display_state f =
  let window_display_state = F.buffer_window_display_state () in
  Exn.protect ~f
    ~finally:(fun () -> F.buffer_restore_window_display_state window_display_state)
;;

let set_multibyte = F.set_buffer_multibyte

let enable_multibyte_characters =
  Var.create Q.enable_multibyte_characters Value.Type.bool
;;

let is_multibyte () = value_exn enable_multibyte_characters

let rename_exn ?(unique = false) () ~name = F.rename_buffer name unique

let set_text_property ?start ?end_ property_name property_value =
  F.put_text_property
    (or_point_min start)
    (or_point_max end_)
    (property_name  |> Text.Property_name.name)
    (property_value |> Text.Property_name.to_value property_name);
;;

(* The [*_staged] functions are special-cased for performance. *)

let set_text_property_staged property_name property_value =
  let property_value = property_value |> Text.Property_name.to_value property_name in
  let property_name  = property_name  |> Text.Property_name.name_as_value          in
  stage (fun ~start ~end_ ->
    Symbol.funcall_int_int_value_value_unit Q.put_text_property
      start
      end_
      property_name
      property_value);
;;

let set_text_properties ?start ?end_ properties =
  F.set_text_properties
    (or_point_min start)
    (or_point_max end_)
    (properties |> Text.Property.to_property_list)
;;

let set_text_properties_staged properties =
  let properties = properties |> Text.Property.to_property_list |> Value.list in
  stage (fun ~start ~end_  ->
    Symbol.funcall_int_int_value_unit Q.set_text_properties
      start
      end_
      properties)
;;

let add_text_properties ?start ?end_ properties =
  F.add_text_properties
    (or_point_min start)
    (or_point_max end_)
    (properties |> Text.Property.to_property_list)
;;

let add_text_properties_staged properties =
  let properties = properties |> Text.Property.to_property_list |> Value.list in
  stage (fun ~start ~end_  ->
    Symbol.funcall_int_int_value_unit Q.add_text_properties
      start
      end_
      properties)
;;

let text_property_is_present ?start ?end_ property_name =
  Value.is_not_nil
    (F.text_property_not_all
       (or_point_min start)
       (or_point_max end_)
       (property_name |> Text.Property_name.name)
       Value.nil)
;;

let set_marker_position = F.set_marker

let mark = F.mark_marker

let set_mark = F.set_mark

let mark_active = Var.create Q.mark_active Value.Type.bool

let mark_is_active () = value_exn mark_active

let deactivate_mark = F.deactivate_mark

let make_buffer_local var =
  add_gc_root (var |> Var.symbol_as_value);
  F.make_local_variable (var |> Var.symbol)
;;

let is_buffer_local var = F.local_variable_p (var |> Var.symbol)

let is_buffer_local_if_set var = F.local_variable_if_set_p (var |> Var.symbol)

let buffer_local_variables () =
  F.buffer_local_variables ()
  |> List.map ~f:(fun value ->
    if Value.is_symbol value
    then (value |> Symbol.of_value_exn, None)
    else (Value.car_exn value |> Symbol.of_value_exn, Some (Value.cdr_exn value)))
;;

let kill_buffer_local var = F.kill_local_variable (var |> Var.symbol)

let syntax_class char_code = F.char_syntax char_code |> Syntax_table.Class.of_char_code_exn

let syntax_table = F.syntax_table

let set_syntax_table = F.set_syntax_table

let local_keymap = F.current_local_map

let set_local_keymap = F.use_local_map

let minor_mode_keymaps = F.current_minor_mode_maps

let delete_lines_matching ?start ?end_ regexp =
  F.flush_lines regexp (or_point_min start) (or_point_max end_)
;;

let sort_lines ?start ?end_ () =
  F.sort_lines Value.nil (or_point_min start) (or_point_max end_)
;;

let delete_duplicate_lines ?start ?end_ () =
  F.delete_duplicate_lines (or_point_min start) (or_point_max end_)
;;

let indent_region ?start ?end_ () =
  Echo_area.inhibit_messages (fun () ->
    F.indent_region (or_point_min start) (or_point_max end_));
;;

let revert ?(confirm = false) () = F.revert_buffer Value.nil (not confirm)

let set_revert_buffer_function f =
  set_value (Var.create Q.revert_buffer_function Function.type_)
    (Function.create [%here]
       ~args:[ Q.ignore_auto; Q.noconfirm ]
       (function
         | [| _; noconfirm |] ->
           f ~confirm:(noconfirm |> Value.to_bool |> not);
           Value.nil
         | _ -> assert false))
;;
