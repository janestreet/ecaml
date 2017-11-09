open! Core_kernel
open! Import

include (Current_buffer0
         : (module type of Current_buffer0
             with module Buffer := Buffer0))

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
  Symbol.funcall0_i (Major_mode.change_command major_mode)
;;

let bury () = Symbol.funcall0_i Q.bury_buffer

let directory = Var.create Q.default_directory Value.Type.string

let is_modified () = Symbol.funcall0 Q.buffer_modified_p |> Value.to_bool

let set_modified bool = Symbol.funcall1_i Q.set_buffer_modified_p (bool |> Value.of_bool)

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

let file_name_var = Var.create Q.buffer_file_name Value.Type.string

let transient_mark_mode = Var.create Q.transient_mark_mode Value.Type.bool

let buffer_undo_list = Var.create Q.buffer_undo_list Value.Type.value

let is_undo_enabled () = not (Value.eq (value_exn buffer_undo_list) Value.t)

let set_undo_enabled bool =
  Symbol.funcall0_i (if bool
                     then Q.buffer_enable_undo
                     else Q.buffer_disable_undo);
;;

let undo_list () = value_exn buffer_undo_list

let undo n = Symbol.funcall1_i Q.undo (n |> Value.of_int_exn)

let add_undo_boundary () = Symbol.funcall0_i Q.undo_boundary

let end_value option =
  Position.to_value
    (match option with
     | Some x -> x
     | None -> Point.max ())
;;

let start_value option =
  Position.to_value
    (match option with
     | Some x -> x
     | None -> Point.min ())
;;

let contents ?start ?end_ ?(text_properties = false) () =
  Symbol.funcall2
    (if text_properties
     then Q.buffer_substring
     else Q.buffer_substring_no_properties)
    (start_value start)
    (end_value end_)
  |> Text.of_value_exn
;;

let kill () = Symbol.funcall0_i Q.kill_buffer

let save () = Symbol.funcall0_i Q.save_buffer

let erase () = Symbol.funcall0_i Q.erase_buffer

let delete_region ~start ~end_ =
  Symbol.funcall2_i Q.delete_region
    (start |> Position.to_value)
    (end_  |> Position.to_value)
;;

let kill_region ~start ~end_ =
  Symbol.funcall2_i Q.kill_region
    (start |> Position.to_value)
    (end_  |> Position.to_value)
;;

let save_excursion f =
  let r = ref None in
  let f =
    Function.create [%here] ~args:[]
      (function
        | [| |] -> r := Some (f ()); Value.nil
        | _ -> assert false)
  in
  ignore (
    Form.eval (
      Form.list
        [ Q.save_excursion |> Form.symbol
        ; Form.list [ Q.funcall |> Form.symbol
                    ; f |> Function.to_value |> Form.quote ]])
    : Value.t);
  match !r with
  | None -> assert false
  | Some a -> a
;;

let set_multibyte bool = Symbol.funcall1_i Q.set_buffer_multibyte (bool |> Value.of_bool)

let enable_multibyte_characters =
  Var.create Q.enable_multibyte_characters Value.Type.bool
;;

let is_multibyte () = value_exn enable_multibyte_characters

let rename_exn ?(unique = false) () ~name =
  Symbol.funcall2_i Q.rename_buffer
    (name |> Value.of_utf8_bytes)
    (unique |> Value.of_bool)
;;

let set_text_property ?start ?end_ property_name property_value =
  Symbol.funcall4_i Q.put_text_property
    (start_value start)
    (end_value end_)
    (property_name  |> Text.Property_name.name_as_value)
    (property_value |> Text.Property_name.to_value property_name);
;;

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
  Symbol.funcall3_i Q.set_text_properties
    (start_value start)
    (end_value end_)
    (properties |> Text.Property.to_property_list |> Value.list)
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
  Symbol.funcall3_i Q.add_text_properties
    (start_value start)
    (end_value end_)
    (properties |> Text.Property.to_property_list |> Value.list)
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
    (Symbol.funcall4 Q.text_property_not_all
       (start_value start)
       (end_value end_)
       (property_name |> Text.Property_name.name_as_value)
       Value.nil)
;;

let set_marker_position marker position =
  Symbol.funcall2_i Q.set_marker
    (marker |> Marker.to_value)
    (position |> Position.to_value)
;;

let mark () = Symbol.funcall0 Q.mark_marker |> Marker.of_value_exn

let set_mark position = Symbol.funcall1_i Q.set_mark (position |> Position.to_value)

let mark_active = Var.create Q.mark_active Value.Type.bool

let mark_is_active () = value_exn mark_active

let deactivate_mark () = Symbol.funcall0_i Q.deactivate_mark

let make_buffer_local var =
  let symbol = var |> Var.symbol_as_value in
  add_gc_root symbol;
  Symbol.funcall1_i Q.make_local_variable symbol
;;

let is_buffer_local var =
  Symbol.funcall1 Q.local_variable_p (var |> Var.symbol_as_value)
  |> Value.to_bool
;;

let is_buffer_local_if_set var =
  Symbol.funcall1 Q.local_variable_if_set_p (var |> Var.symbol_as_value)
  |> Value.to_bool
;;

let buffer_local_variables () =
  Symbol.funcall0 Q.buffer_local_variables
  |> Value.to_list_exn ~f:(fun value ->
    if Value.is_symbol value
    then (value |> Symbol.of_value_exn, None)
    else (Value.car_exn value |> Symbol.of_value_exn, Some (Value.cdr_exn value)))
;;

let kill_buffer_local var =
  Symbol.funcall1_i Q.kill_local_variable (var |> Var.symbol_as_value)
;;

let syntax_class char_code =
  Symbol.funcall1 Q.char_syntax (char_code |> Char_code.to_value)
  |> Char_code.of_value_exn
  |> Syntax_table.Class.of_char_code_exn
;;

let syntax_table () = Symbol.funcall0 Q.syntax_table |> Syntax_table.of_value_exn

let set_syntax_table syntax_table =
  Symbol.funcall1_i Q.set_syntax_table (syntax_table |> Syntax_table.to_value)
;;

let local_keymap () =
  let result = Symbol.funcall0 Q.current_local_map in
  if Value.is_nil result
  then None
  else Some (result |> Keymap.of_value_exn)
;;

let set_local_keymap keymap =
  Symbol.funcall1_i Q.use_local_map (keymap |> Keymap.to_value)
;;

let minor_mode_keymaps () =
  Symbol.funcall0 Q.current_minor_mode_maps
  |> Value.to_list_exn ~f:Keymap.of_value_exn
;;

let delete_lines_matching ?start ?end_ regexp =
  Symbol.funcall3_i Q.flush_lines
    (regexp |> Regexp.to_value)
    (start_value start)
    (end_value end_)
;;

let sort_lines ?start ?end_ () =
  Symbol.funcall3_i Q.sort_lines
    Value.nil
    (start_value start)
    (end_value end_)
;;

let delete_duplicate_lines ?start ?end_ () =
  Symbol.funcall2_i Q.delete_duplicate_lines
    (start_value start)
    (end_value end_)
;;

let indent_region ?start ?end_ () =
  Echo_area.inhibit_messages (fun () ->
    Symbol.funcall2_i Q.indent_region
      (start_value start)
      (end_value end_));
;;

let revert ?(confirm = false) () =
  Symbol.funcall2_i
    Q.revert_buffer
    Value.nil
    (confirm |> not |> Value.of_bool)
;;

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
