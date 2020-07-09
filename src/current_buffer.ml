open! Core_kernel
open! Import

module Q = struct
  include Q

  let add_text_properties = "add-text-properties" |> Symbol.intern
  let put_text_property = "put-text-property" |> Symbol.intern
  let replace_buffer_contents = "replace-buffer-contents" |> Symbol.intern
  let set_text_properties = "set-text-properties" |> Symbol.intern
end

include Current_buffer0

let get_buffer_local = Buffer_local.Private.get_in_current_buffer
let get_buffer_local_exn = Buffer_local.Private.get_in_current_buffer_exn
let set_buffer_local = Buffer_local.Private.set_in_current_buffer
let set_buffer_local_temporarily = Buffer_local.Private.set_temporarily_in_current_buffer

let set_temporarily_to_temp_buffer sync_or_async f =
  let t = Buffer.create ~name:"*temp-buffer*" in
  Sync_or_async.protect
    [%here]
    sync_or_async
    ~f:(fun () -> set_temporarily sync_or_async t ~f)
    ~finally:(fun () -> Buffer.Blocking.kill t)
;;

let major_mode () =
  Major_mode.find_or_wrap_existing [%here] (get_buffer_local Major_mode.major_mode_var)
;;

let set_auto_mode =
  let set_auto_mode = Funcall.Wrap.("set-auto-mode" <: nil_or bool @-> return nil) in
  fun ?keep_mode_if_same () ->
    Value.Private.run_outside_async [%here] (fun () -> set_auto_mode keep_mode_if_same)
;;

let bury = Funcall.Wrap.("bury-buffer" <: nullary @-> return nil)
let directory = Buffer_local.Wrap.("default-directory" <: nil_or string)
let describe_mode = Funcall.Wrap.("describe-mode" <: nullary @-> return nil)
let is_modified = Funcall.Wrap.("buffer-modified-p" <: nullary @-> return bool)
let set_modified = Funcall.Wrap.("set-buffer-modified-p" <: bool @-> return nil)
let fill_column = Buffer_local.Wrap.("fill-column" <: int)
let paragraph_start = Var.Wrap.("paragraph-start" <: Regexp.t)
let paragraph_separate = Var.Wrap.("paragraph-separate" <: Regexp.t)
let read_only = Buffer_local.Wrap.("buffer-read-only" <: bool)
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

let file_name_var = Buffer_local.Wrap.("buffer-file-name" <: nil_or string)

module Coding_system = struct
  module T = struct
    type t =
      | Utf_8
      | Utf_8_unix
    [@@deriving enumerate, sexp]
  end

  include T

  let type_ =
    Value.Type.enum
      [%sexp "buffer-file-coding-system"]
      (module T)
      (function
        | Utf_8 -> "utf-8" |> Value.intern
        | Utf_8_unix -> "utf-8-unix" |> Value.intern)
  ;;

  let t = type_
end

let file_coding_system =
  Buffer_local.Wrap.("buffer-file-coding-system" <: nil_or Coding_system.t)
;;

let transient_mark_mode = Customization.Wrap.("transient-mark-mode" <: bool)

let () =
  (* Emacs, by default, turns [transient-mark-mode] off in batch mode and on in
     interactive mode.  Who knows why.  The difference causes pointless rough edges in our
     integration tests, which run in batch mode and where we expect behavior to match
     interactive mode.  So, we turn [transient-mark-mode] on in tests. *)
  if am_running_test then Customization.set_value transient_mark_mode true
;;

let buffer_undo_list = Buffer_local.Wrap.("buffer-undo-list" <: value)
let is_undo_enabled () = not (Value.eq (get_buffer_local buffer_undo_list) Value.t)
let buffer_disable_undo = Funcall.Wrap.("buffer-disable-undo" <: nullary @-> return nil)
let buffer_enable_undo = Funcall.Wrap.("buffer-enable-undo" <: nullary @-> return nil)

let set_undo_enabled bool =
  if bool then buffer_enable_undo () else buffer_disable_undo ()
;;

let undo_list () = get_buffer_local buffer_undo_list
let undo = Funcall.Wrap.("undo" <: int @-> return nil)
let add_undo_boundary = Funcall.Wrap.("undo-boundary" <: nullary @-> return nil)

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

let buffer_substring =
  Funcall.Wrap.("buffer-substring" <: Position.t @-> Position.t @-> return Text.t)
;;

let buffer_substring_no_properties =
  Funcall.Wrap.(
    "buffer-substring-no-properties" <: Position.t @-> Position.t @-> return Text.t)
;;

let contents ?start ?end_ ?(text_properties = false) () =
  (if text_properties then buffer_substring else buffer_substring_no_properties)
    (or_point_min start)
    (or_point_max end_)
;;

let kill =
  let kill_buffer = Funcall.Wrap.("kill-buffer" <: nullary @-> return nil) in
  fun () ->
    Value.Private.run_outside_async [%here] ~allowed_in_background:true kill_buffer
;;

let save =
  let save_buffer = Funcall.Wrap.("save-buffer" <: nullary @-> return nil) in
  fun () ->
    Value.Private.run_outside_async [%here] ~allowed_in_background:true save_buffer
;;

let erase = Funcall.Wrap.("erase-buffer" <: nullary @-> return nil)

let delete_region =
  Funcall.Wrap.("delete-region" <: Position.t @-> Position.t @-> return nil)
;;

let delete_region ~start ~end_ = delete_region start end_

let kill_region =
  Funcall.Wrap.("kill-region" <: Position.t @-> Position.t @-> return nil)
;;

let kill_region ~start ~end_ = kill_region start end_
let widen = Funcall.Wrap.("widen" <: nullary @-> return nil)
let save_current_buffer f = Save_wrappers.save_current_buffer f
let save_excursion f = Save_wrappers.save_excursion f
let save_mark_and_excursion f = Save_wrappers.save_mark_and_excursion f
let save_restriction f = Save_wrappers.save_restriction f
let set_multibyte = Funcall.Wrap.("set-buffer-multibyte" <: bool @-> return nil)

let enable_multibyte_characters =
  Buffer_local.Wrap.("enable-multibyte-characters" <: bool)
;;

let is_multibyte () = get_buffer_local enable_multibyte_characters
let rename_buffer = Funcall.Wrap.("rename-buffer" <: string @-> bool @-> return nil)
let rename_exn ?(unique = false) () ~name = rename_buffer name unique

let put_text_property =
  Funcall.Wrap.(
    "put-text-property"
    <: Position.t @-> Position.t @-> Symbol.t @-> value @-> return nil)
;;

let set_text_property ?start ?end_ property_name property_value =
  put_text_property
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

let set_text_properties =
  Funcall.Wrap.(
    "set-text-properties" <: Position.t @-> Position.t @-> list value @-> return nil)
;;

let set_text_properties ?start ?end_ properties =
  set_text_properties
    (or_point_min start)
    (or_point_max end_)
    (properties |> Text.Property.to_property_list)
;;

let set_text_properties_staged properties =
  let properties = properties |> Text.Property.to_property_list |> Value.list in
  stage (fun ~start ~end_ ->
    Symbol.funcall_int_int_value_unit Q.set_text_properties start end_ properties)
;;

let get_text_property =
  Funcall.Wrap.("get-text-property" <: Position.t @-> value @-> return (nil_or value))
;;

let get_text_property at property_name =
  get_text_property at (property_name |> Text.Property_name.name_as_value)
  |> Option.map ~f:(Text.Property_name.of_value_exn property_name)
;;

let add_text_properties =
  Funcall.Wrap.(
    "add-text-properties" <: Position.t @-> Position.t @-> list value @-> return nil)
;;

let add_text_properties ?start ?end_ properties =
  add_text_properties
    (or_point_min start)
    (or_point_max end_)
    (properties |> Text.Property.to_property_list)
;;

let add_text_properties_staged properties =
  let properties = properties |> Text.Property.to_property_list |> Value.list in
  stage (fun ~start ~end_ ->
    Symbol.funcall_int_int_value_unit Q.add_text_properties start end_ properties)
;;

let text_property_not_all =
  Funcall.Wrap.(
    "text-property-not-all"
    <: Position.t @-> Position.t @-> Symbol.t @-> value @-> return value)
;;

let text_property_is_present ?start ?end_ property_name =
  Value.is_not_nil
    (text_property_not_all
       (or_point_min start)
       (or_point_max end_)
       (property_name |> Text.Property_name.name)
       Value.nil)
;;

let set_marker_position =
  Funcall.Wrap.("set-marker" <: Marker.t @-> Position.t @-> return nil)
;;

let mark = Funcall.Wrap.("mark-marker" <: nullary @-> return Marker.t)
let set_mark = Funcall.Wrap.("set-mark" <: Position.t @-> return nil)
let mark_active = Buffer_local.Wrap.("mark-active" <: bool)
let mark_is_active () = get_buffer_local mark_active
let deactivate_mark = Funcall.Wrap.("deactivate-mark" <: nullary @-> return nil)
let region_beginning = Funcall.Wrap.("region-beginning" <: nullary @-> return Position.t)
let region_end = Funcall.Wrap.("region-end" <: nullary @-> return Position.t)

let active_region () =
  if mark_is_active () then Some (region_beginning (), region_end ()) else None
;;

let make_local_variable = Funcall.Wrap.("make-local-variable" <: Symbol.t @-> return nil)

let make_buffer_local var =
  add_gc_root (var |> Var.symbol_as_value);
  make_local_variable (var |> Var.symbol)
;;

let local_variable_p = Funcall.Wrap.("local-variable-p" <: Symbol.t @-> return bool)
let is_buffer_local var = local_variable_p (var |> Var.symbol)

let local_variable_if_set_p =
  Funcall.Wrap.("local-variable-if-set-p" <: Symbol.t @-> return bool)
;;

let is_buffer_local_if_set var = local_variable_if_set_p (var |> Var.symbol)

let buffer_local_variables =
  Funcall.Wrap.("buffer-local-variables" <: nullary @-> return (list value))
;;

let buffer_local_variables () =
  buffer_local_variables ()
  |> List.map ~f:(fun value ->
    if Value.is_symbol value
    then value |> Symbol.of_value_exn, None
    else Value.car_exn value |> Symbol.of_value_exn, Some (Value.cdr_exn value))
;;

let kill_local_variable = Funcall.Wrap.("kill-local-variable" <: Symbol.t @-> return nil)
let kill_buffer_local var = kill_local_variable (var |> Var.symbol)
let char_syntax = Funcall.Wrap.("char-syntax" <: Char_code.t @-> return Char_code.t)
let syntax_class char_code = char_syntax char_code |> Syntax_table.Class.of_char_code_exn
let syntax_table = Funcall.Wrap.("syntax-table" <: nullary @-> return Syntax_table.t)
let set_syntax_table = Funcall.Wrap.("set-syntax-table" <: Syntax_table.t @-> return nil)

let local_keymap =
  Funcall.Wrap.("current-local-map" <: nullary @-> return (nil_or Keymap.t))
;;

let set_local_keymap = Funcall.Wrap.("use-local-map" <: Keymap.t @-> return nil)

let minor_mode_keymaps =
  Funcall.Wrap.("current-minor-mode-maps" <: nullary @-> return (list Keymap.t))
;;

let flush_lines =
  Funcall.Wrap.("flush-lines" <: Regexp.t @-> Position.t @-> Position.t @-> return nil)
;;

let delete_lines_matching ?start ?end_ regexp =
  flush_lines regexp (or_point_min start) (or_point_max end_)
;;

let sort_lines =
  Funcall.Wrap.("sort-lines" <: value @-> Position.t @-> Position.t @-> return nil)
;;

let sort_lines ?start ?end_ () =
  sort_lines Value.nil (or_point_min start) (or_point_max end_)
;;

let delete_duplicate_lines =
  Funcall.Wrap.("delete-duplicate-lines" <: Position.t @-> Position.t @-> return nil)
;;

let delete_duplicate_lines ?start ?end_ () =
  delete_duplicate_lines (or_point_min start) (or_point_max end_)
;;

let indent_region =
  Funcall.Wrap.("indent-region" <: Position.t @-> Position.t @-> return nil)
;;

let indent_region ?start ?end_ () =
  Echo_area.inhibit_messages Sync (fun () ->
    indent_region (or_point_min start) (or_point_max end_))
;;

let change_major_mode major_mode = Major_mode.change_to major_mode ~in_:(get ())
let revert ?confirm () = Buffer.revert ?confirm (get ())

let revert_buffer_function =
  Buffer_local.Wrap.(
    let ( <: ) = ( <: ) ~make_buffer_local_always:true in
    "revert-buffer-function" <: Function.t)
;;

let set_revert_buffer_function here returns f =
  set_buffer_local
    revert_buffer_function
    (Defun.lambda
       here
       returns
       (let%map_open.Defun () = return ()
        and () = required "ignore-auto" ignored
        and noconfirm = required "noconfirm" bool in
        f ~confirm:(not noconfirm)))
;;

let replace_buffer_contents =
  if not (Symbol.function_is_defined Q.replace_buffer_contents)
  then
    Or_error.error_s
      [%message "function not defined" ~symbol:(Q.replace_buffer_contents : Symbol.t)]
  else Ok Funcall.Wrap.("replace-buffer-contents" <: Buffer.t @-> return nil)
;;

let size = Funcall.Wrap.("buffer-size" <: nullary @-> return int)
let truncate_lines = Buffer_local.Wrap.("truncate-lines" <: bool)

let chars_modified_tick =
  Funcall.Wrap.("buffer-chars-modified-tick" <: nullary @-> return Modified_tick.t)
;;

let append_to string =
  let point_max_before = Point.max () in
  save_excursion Sync (fun () ->
    Point.goto_max ();
    Point.insert string);
  let point_max_after = Point.max () in
  if Position.equal (Point.get ()) point_max_before then Point.goto_max ();
  List.iter
    (Buffer.displayed_in (get ()))
    ~f:(fun window ->
      if Position.equal (Window.point_exn window) point_max_before
      then Window.set_point_exn window point_max_after)
;;

let inhibit_read_only = Var.Wrap.("inhibit-read-only" <: bool)

let inhibit_read_only sync_or_async f =
  set_value_temporarily sync_or_async inhibit_read_only true ~f
;;

let position_of_line_and_column line_and_column =
  save_excursion Sync (fun () ->
    Point.goto_line_and_column line_and_column;
    Point.get ())
;;

let line_and_column_of_position position =
  save_excursion Sync (fun () ->
    Point.goto_char position;
    Point.get_line_and_column ())
;;

let replace_string ?start ?end_ ~from ~to_ () =
  let end_ = or_point_max end_ in
  save_excursion Sync (fun () ->
    Point.goto_char (or_point_min start);
    while Point.search_forward from ~bound:end_ ~update_last_match:true do
      Regexp.Last_match.replace to_
    done)
;;
