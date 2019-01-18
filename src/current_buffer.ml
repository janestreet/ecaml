open! Core_kernel
open! Import

module Q = struct
  include Current_buffer0.Q

  let buffer_chars_modified_tick = "buffer-chars-modified-tick" |> Symbol.intern
  and buffer_disable_undo = "buffer-disable-undo" |> Symbol.intern
  and buffer_enable_undo = "buffer-enable-undo" |> Symbol.intern
  and buffer_modified_p = "buffer-modified-p" |> Symbol.intern
  and buffer_restore_window_display_state =
    "Buffer.restore-window-display-state" |> Symbol.intern
  and buffer_size = "buffer-size" |> Symbol.intern
  and buffer_substring = "buffer-substring" |> Symbol.intern
  and buffer_substring_no_properties = "buffer-substring-no-properties" |> Symbol.intern
  and buffer_undo_list = "buffer-undo-list" |> Symbol.intern
  and buffer_window_display_state = "Buffer.window-display-state" |> Symbol.intern
  and bury_buffer = "bury-buffer" |> Symbol.intern
  and char_syntax = "char-syntax" |> Symbol.intern
  and current_local_map = "current-local-map" |> Symbol.intern
  and current_minor_mode_maps = "current-minor-mode-maps" |> Symbol.intern
  and deactivate_mark = "deactivate-mark" |> Symbol.intern
  and default_directory = "default-directory" |> Symbol.intern
  and delete_duplicate_lines = "delete-duplicate-lines" |> Symbol.intern
  and delete_region = "delete-region" |> Symbol.intern
  and describe_mode = "describe-mode" |> Symbol.intern
  and enable_multibyte_characters = "enable-multibyte-characters" |> Symbol.intern
  and erase_buffer = "erase-buffer" |> Symbol.intern
  and fill_column = "fill-column" |> Symbol.intern
  and flush_lines = "flush-lines" |> Symbol.intern
  and get_text_property = "get-text-property" |> Symbol.intern
  and ignore_auto = "ignore-auto" |> Symbol.intern
  and indent_region = "indent-region" |> Symbol.intern
  and kill_local_variable = "kill-local-variable" |> Symbol.intern
  and kill_region = "kill-region" |> Symbol.intern
  and local_variable_if_set_p = "local-variable-if-set-p" |> Symbol.intern
  and local_variable_p = "local-variable-p" |> Symbol.intern
  and make_local_variable = "make-local-variable" |> Symbol.intern
  and mark_active = "mark-active" |> Symbol.intern
  and mark_marker = "mark-marker" |> Symbol.intern
  and noconfirm = "noconfirm" |> Symbol.intern
  and paragraph_separate = "paragraph-separate" |> Symbol.intern
  and paragraph_start = "paragraph-start" |> Symbol.intern
  and region_beginning = "region-beginning" |> Symbol.intern
  and region_end = "region-end" |> Symbol.intern
  and rename_buffer = "rename-buffer" |> Symbol.intern
  and revert_buffer = "revert-buffer" |> Symbol.intern
  and revert_buffer_function = "revert-buffer-function" |> Symbol.intern
  and save_buffer = "save-buffer" |> Symbol.intern
  and set_auto_mode = "set-auto-mode" |> Symbol.intern
  and set_buffer_modified_p = "set-buffer-modified-p" |> Symbol.intern
  and set_buffer_multibyte = "set-buffer-multibyte" |> Symbol.intern
  and set_mark = "set-mark" |> Symbol.intern
  and set_syntax_table = "set-syntax-table" |> Symbol.intern
  and sort_lines = "sort-lines" |> Symbol.intern
  and syntax_table = "syntax-table" |> Symbol.intern
  and text_property_not_all = "text-property-not-all" |> Symbol.intern
  and transient_mark_mode = "transient-mark-mode" |> Symbol.intern
  and undo = "undo" |> Symbol.intern
  and undo_boundary = "undo-boundary" |> Symbol.intern
  and use_local_map = "use-local-map" |> Symbol.intern
  and widen = "widen" |> Symbol.intern
  and replace_buffer_contents = "replace-buffer-contents" |> Symbol.intern
end

include (Current_buffer0 : Current_buffer0_intf.Current_buffer0 with module Q := Q)

module Window_display_state = struct
  include Value.Make_subtype (struct
      let here = [%here]
      let name = "Buffer.window-display-state"

      let is_in_subtype =
        let open Value in
        is_cons
          ~car:Position.is_in_subtype
          ~cdr:(is_cons ~car:Position.is_in_subtype ~cdr:Position.is_in_subtype)
      ;;
    end)
end

module F = struct
  open Funcall

  let buffer_chars_modified_tick =
    Q.buffer_chars_modified_tick <: nullary @-> return Modified_tick.type_
  and buffer_disable_undo = Q.buffer_disable_undo <: nullary @-> return nil
  and buffer_enable_undo = Q.buffer_enable_undo <: nullary @-> return nil
  and buffer_size = Q.buffer_size <: nullary @-> return int
  and bury_buffer = Q.bury_buffer <: nullary @-> return nil
  and deactivate_mark = Q.deactivate_mark <: nullary @-> return nil
  and describe_mode = Q.describe_mode <: nullary @-> return nil
  and erase_buffer = Q.erase_buffer <: nullary @-> return nil
  and kill_buffer = Q.kill_buffer <: nullary @-> return nil
  and save_buffer = Q.save_buffer <: nullary @-> return nil
  and undo_boundary = Q.undo_boundary <: nullary @-> return nil
  and widen = Q.widen <: nullary @-> return nil
  and buffer_local_variables =
    Q.buffer_local_variables <: nullary @-> return (list value)
  and buffer_modified_p = Q.buffer_modified_p <: nullary @-> return bool
  and current_local_map = Q.current_local_map <: nullary @-> return (option Keymap.type_)
  and current_minor_mode_maps =
    Q.current_minor_mode_maps <: nullary @-> return (list Keymap.type_)
  and get_text_property =
    Q.get_text_property <: Position.type_ @-> value @-> return (option value)
  and mark_marker = Q.mark_marker <: nullary @-> return Marker.type_
  and syntax_table = Q.syntax_table <: nullary @-> return Syntax_table.type_
  and delete_region = Q.delete_region <: Position.type_ @-> Position.type_ @-> return nil
  and indent_region = Q.indent_region <: Position.type_ @-> Position.type_ @-> return nil
  and kill_local_variable = Q.kill_local_variable <: Symbol.type_ @-> return nil
  and kill_region = Q.kill_region <: Position.type_ @-> Position.type_ @-> return nil
  and make_local_variable = Q.make_local_variable <: Symbol.type_ @-> return nil
  and region_beginning = Q.region_beginning <: nullary @-> return Position.type_
  and region_end = Q.region_end <: nullary @-> return Position.type_
  and rename_buffer = Q.rename_buffer <: string @-> bool @-> return nil
  and revert_buffer = Q.revert_buffer <: value @-> bool @-> return nil
  and set_buffer_modified_p = Q.set_buffer_modified_p <: bool @-> return nil
  and set_buffer_multibyte = Q.set_buffer_multibyte <: bool @-> return nil
  and set_auto_mode = Q.set_auto_mode <: option bool @-> return nil
  and set_mark = Q.set_mark <: Position.type_ @-> return nil
  and set_marker = Q.set_marker <: Marker.type_ @-> Position.type_ @-> return nil
  and set_syntax_table = Q.set_syntax_table <: Syntax_table.type_ @-> return nil
  and undo = Q.undo <: int @-> return nil
  and use_local_map = Q.use_local_map <: Keymap.type_ @-> return nil
  and char_syntax = Q.char_syntax <: Char_code.type_ @-> return Char_code.type_
  and local_variable_p = Q.local_variable_p <: Symbol.type_ @-> return bool
  and local_variable_if_set_p = Q.local_variable_if_set_p <: Symbol.type_ @-> return bool
  and buffer_window_display_state =
    Q.buffer_window_display_state <: nullary @-> return Window_display_state.type_
  and buffer_restore_window_display_state =
    Q.buffer_restore_window_display_state <: Window_display_state.type_ @-> return nil

  and add_text_properties =
    Q.add_text_properties
    <: Position.type_ @-> Position.type_ @-> list value @-> return nil

  and buffer_substring =
    Q.buffer_substring <: Position.type_ @-> Position.type_ @-> return Text.type_

  and buffer_substring_no_properties =
    Q.buffer_substring_no_properties
    <: Position.type_ @-> Position.type_ @-> return Text.type_

  and delete_duplicate_lines =
    Q.delete_duplicate_lines <: Position.type_ @-> Position.type_ @-> return nil
  and flush_lines =
    Q.flush_lines <: Regexp.type_ @-> Position.type_ @-> Position.type_ @-> return nil

  and put_text_property =
    Q.put_text_property
    <: Position.type_ @-> Position.type_ @-> Symbol.type_ @-> value @-> return nil

  and set_text_properties =
    Q.set_text_properties
    <: Position.type_ @-> Position.type_ @-> list value @-> return nil

  and sort_lines =
    Q.sort_lines <: value @-> Position.type_ @-> Position.type_ @-> return nil

  and text_property_not_all =
    Q.text_property_not_all
    <: Position.type_ @-> Position.type_ @-> Symbol.type_ @-> value @-> return value

  and replace_buffer_contents = Q.replace_buffer_contents <: Buffer.type_ @-> return nil
end

let get_buffer_local = Buffer_local.Private.get_in_current_buffer
let get_buffer_local_exn = Buffer_local.Private.get_in_current_buffer_exn
let set_buffer_local = Buffer_local.Private.set_in_current_buffer

let set_temporarily_to_temp_buffer f =
  let t = Buffer.create ~name:"*temp-buffer*" in
  protect ~f:(fun () -> set_temporarily t ~f) ~finally:(fun () -> Buffer.kill t)
;;

let major_mode () =
  Major_mode.find_or_wrap_existing [%here] (get_buffer_local Major_mode.major_mode_var)
;;

let change_major_mode major_mode =
  Funcall.(Major_mode.symbol major_mode <: nullary @-> return nil) ()
;;

let set_auto_mode ?keep_mode_if_same () = F.set_auto_mode keep_mode_if_same
let bury = F.bury_buffer
let directory = Buffer_local.wrap_existing Q.default_directory Value.Type.string
let describe_mode = F.describe_mode
let is_modified = F.buffer_modified_p
let set_modified = F.set_buffer_modified_p
let fill_column = Buffer_local.wrap_existing Q.fill_column Value.Type.int
let paragraph_start = Var.create Q.paragraph_start Regexp.type_
let paragraph_separate = Var.create Q.paragraph_separate Regexp.type_
let read_only = Buffer_local.wrap_existing Q.buffer_read_only Value.Type.bool
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

let file_name_var =
  Buffer_local.wrap_existing Q.buffer_file_name Value.Type.(option string)
;;

let transient_mark_mode = Var.create Q.transient_mark_mode Value.Type.bool
let buffer_undo_list = Buffer_local.wrap_existing Q.buffer_undo_list Value.Type.value
let is_undo_enabled () = not (Value.eq (get_buffer_local buffer_undo_list) Value.t)

let set_undo_enabled bool =
  if bool then F.buffer_enable_undo () else F.buffer_disable_undo ()
;;

let undo_list () = get_buffer_local buffer_undo_list
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
  (if text_properties then F.buffer_substring else F.buffer_substring_no_properties)
    (or_point_min start)
    (or_point_max end_)
;;

let kill = F.kill_buffer
let save = F.save_buffer
let erase = F.erase_buffer
let delete_region ~start ~end_ = F.delete_region start end_
let kill_region ~start ~end_ = F.kill_region start end_
let widen = F.widen
let save_current_buffer f = Save_wrappers.save_current_buffer f
let save_excursion f = Save_wrappers.save_excursion f
let save_mark_and_excursion f = Save_wrappers.save_mark_and_excursion f
let save_restriction f = Save_wrappers.save_restriction f

let save_window_display_state f =
  let window_display_state = F.buffer_window_display_state () in
  Exn.protect ~f ~finally:(fun () ->
    F.buffer_restore_window_display_state window_display_state)
;;

let set_multibyte = F.set_buffer_multibyte

let enable_multibyte_characters =
  Buffer_local.wrap_existing Q.enable_multibyte_characters Value.Type.bool
;;

let is_multibyte () = get_buffer_local enable_multibyte_characters
let rename_exn ?(unique = false) () ~name = F.rename_buffer name unique

let set_text_property ?start ?end_ property_name property_value =
  F.put_text_property
    (or_point_min start)
    (or_point_max end_)
    (property_name |> Text.Property_name.name)
    (property_value |> Text.Property_name.to_value property_name)
;;

(* The [*_staged] functions are special-cased for performance. *)

let set_text_property_staged property_name property_value =
  let property_value = property_value |> Text.Property_name.to_value property_name in
  let property_name = property_name |> Text.Property_name.name_as_value in
  stage (fun ~start ~end_ ->
    Symbol.funcall_int_int_value_value_unit
      Q.put_text_property
      start
      end_
      property_name
      property_value)
;;

let set_text_properties ?start ?end_ properties =
  F.set_text_properties
    (or_point_min start)
    (or_point_max end_)
    (properties |> Text.Property.to_property_list)
;;

let set_text_properties_staged properties =
  let properties = properties |> Text.Property.to_property_list |> Value.list in
  stage (fun ~start ~end_ ->
    Symbol.funcall_int_int_value_unit Q.set_text_properties start end_ properties)
;;

let get_text_property at property_name =
  F.get_text_property at (property_name |> Text.Property_name.name_as_value)
  |> Option.map ~f:(Text.Property_name.of_value_exn property_name)
;;

let add_text_properties ?start ?end_ properties =
  F.add_text_properties
    (or_point_min start)
    (or_point_max end_)
    (properties |> Text.Property.to_property_list)
;;

let add_text_properties_staged properties =
  let properties = properties |> Text.Property.to_property_list |> Value.list in
  stage (fun ~start ~end_ ->
    Symbol.funcall_int_int_value_unit Q.add_text_properties start end_ properties)
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
let mark_active = Buffer_local.wrap_existing Q.mark_active Value.Type.bool
let mark_is_active () = get_buffer_local mark_active
let deactivate_mark = F.deactivate_mark

let active_region () =
  if mark_is_active () then Some (F.region_beginning (), F.region_end ()) else None
;;

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
    then value |> Symbol.of_value_exn, None
    else Value.car_exn value |> Symbol.of_value_exn, Some (Value.cdr_exn value))
;;

let kill_buffer_local var = F.kill_local_variable (var |> Var.symbol)

let syntax_class char_code =
  F.char_syntax char_code |> Syntax_table.Class.of_char_code_exn
;;

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
    F.indent_region (or_point_min start) (or_point_max end_))
;;

let revert ?(confirm = false) () = F.revert_buffer Value.nil (not confirm)

let revert_buffer_function =
  Buffer_local.wrap_existing
    Q.revert_buffer_function
    Function.type_
    ~make_buffer_local_always:true
;;

let set_revert_buffer_function f =
  set_buffer_local
    revert_buffer_function
    (Function.create [%here] ~args:[ Q.ignore_auto; Q.noconfirm ] (function
       | [| _; noconfirm |] ->
         f ~confirm:(noconfirm |> Value.to_bool |> not);
         Value.nil
       | _ -> assert false))
;;

let replace_buffer_contents =
  if not (Symbol.function_is_defined Q.replace_buffer_contents)
  then
    Or_error.error_s
      [%message "function not defined" ~symbol:(Q.replace_buffer_contents : Symbol.t)]
  else Ok F.replace_buffer_contents
;;

let size = F.buffer_size

let truncate_lines =
  Buffer_local.wrap_existing ("truncate-lines" |> Symbol.intern) Value.Type.bool
;;

let chars_modified_tick = F.buffer_chars_modified_tick
