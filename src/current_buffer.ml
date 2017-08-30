open! Core_kernel
open! Import

let get () = Buffer.Private.current ()

let set t = Symbol.funcall1_i Q.set_buffer (t |> Buffer.to_value)

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

let directory () =
  Symbol.value_exn Q.default_directory |> Filename.of_value_exn
;;

let set_directory dir =
  Symbol.set_value Q.default_directory (dir |> Filename.to_value)
;;

let is_modified () = Symbol.funcall0 Q.buffer_modified_p |> Value.to_bool

let set_modified bool = Symbol.funcall1_i Q.set_buffer_modified_p (bool |> Value.of_bool)

let is_read_only () = Symbol.value_exn Q.buffer_read_only |> Value.to_bool

let set_read_only bool = Symbol.set_value Q.buffer_read_only (bool |> Value.of_bool)

let is_undo_enabled () = not (Value.eq (Symbol.value_exn Q.buffer_undo_list) Value.t)

let set_undo_enabled bool =
  Symbol.funcall0_i (if bool
                     then Q.buffer_enable_undo
                     else Q.buffer_disable_undo);
;;

let undo_list () = Symbol.value_exn Q.buffer_undo_list

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
  ignore
    (Form.eval (
       Form.list
         [ Q.save_excursion |> Form.symbol
         ; Form.list [ f |> Function.to_value |> Form.of_value ]])
     : Value.t);
  match !r with
  | None -> assert false
  | Some a -> a
;;

let set_multibyte bool = Symbol.funcall1_i Q.set_buffer_multibyte (bool |> Value.of_bool)

let is_multibyte () = Symbol.value_exn Q.enable_multibyte_characters |> Value.to_bool

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

let get_mark () = Symbol.funcall0 Q.mark_marker |> Marker.of_value_exn

let set_mark position = Symbol.funcall1_i Q.set_mark (position |> Position.to_value)

let mark_is_active () = Symbol.value_exn Q.mark_active |> Value.to_bool

let deactivate_mark () = Symbol.funcall0_i Q.deactivate_mark

module Minor_mode = struct
  type t =
    { function_name : Symbol.t
    ; variable_name : Symbol.t }
  [@@deriving sexp_of]

  let read_only =
    { function_name = Q.read_only_mode
    ; variable_name = Q.buffer_read_only }
  ;;

  let view =
    { function_name = Q.view_mode
    ; variable_name = Q.view_mode }
  ;;

  let is_enabled t = Symbol.value_exn t.variable_name |> Value.to_bool

  let disable t = Symbol.funcall1_i t.function_name (0 |> Value.of_int_exn)
  let enable  t = Symbol.funcall1_i t.function_name (1 |> Value.of_int_exn)
end
