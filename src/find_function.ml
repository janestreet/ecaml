open! Core
open! Import

(* [find-function-search-for-symbol] visits the file that [load-history] names for a
   symbol, and then consults the symbol's [find-function-type-alist] property for how to
   find the definition within that file. For a symbol defined by Ecaml the file is OCaml
   source, so the regexps Emacs would use by default find nothing; instead we install a
   function that moves point to the [Source_code_position.t] recorded by
   [Load_history.add_entry]. *)
let goto_definition symbol type_ =
  let location = Load_history.location_exn symbol type_ in
  Point.goto_char (Position.of_byte_position_exn (location.pos_cnum + 1));
  true
;;

let find_face_definition = "ecaml-find-face-definition" |> Symbol.intern

let () =
  Defun.defun
    find_face_definition
    [%here]
    ~docstring:"Move point to where Ecaml defines SYMBOL as a face, and return t."
    (Returns Value.Type.bool)
    (let%map_open.Defun symbol = required "symbol" Symbol.t in
     goto_definition symbol Face)
;;

let find_function_definition = "ecaml-find-function-definition" |> Symbol.intern

let () =
  Defun.defun
    find_function_definition
    [%here]
    ~docstring:"Move point to where Ecaml defines SYMBOL as a function, and return t."
    (Returns Value.Type.bool)
    (let%map_open.Defun symbol = required "symbol" Symbol.t in
     goto_definition symbol Fun)
;;

let find_variable_definition = "ecaml-find-variable-definition" |> Symbol.intern

let () =
  Defun.defun
    find_variable_definition
    [%here]
    ~docstring:"Move point to where Ecaml defines SYMBOL as a variable, and return t."
    (Returns Value.Type.bool)
    (let%map_open.Defun symbol = required "symbol" Symbol.t in
     goto_definition symbol Var)
;;

let find_function_update_type_alist =
  Funcall.Wrap.(
    "find-function-update-type-alist"
    <: Symbol.t @-> Load_history.Type.t @-> Symbol.t @-> return nil)
;;

let update_find_function_type_alist entries =
  List.iter
    entries
    ~f:(fun ((_ : Source_code_position.t), (entry : Load_history.Entry.t)) ->
      match entry with
      | Face symbol -> find_function_update_type_alist symbol Face find_face_definition
      | Fun symbol -> find_function_update_type_alist symbol Fun find_function_definition
      | Var symbol -> find_function_update_type_alist symbol Var find_variable_definition
      | Autoload _ | Previously_an_autoload _ | Provide _ | Require _ -> ())
;;

let load_history = Var.Wrap.("load-history" <: value)
let append = Funcall.Wrap.("append" <: value @-> value @-> return value)

let update_emacs_with_entries ~chop_prefix ~in_dir =
  let entries = Load_history.take_entries () in
  update_find_function_type_alist entries;
  let addition =
    entries
    |> List.map ~f:(fun ((source_code_position : Source_code_position.t), entry) ->
      String.chop_prefix_exn source_code_position.pos_fname ~prefix:chop_prefix, entry)
    |> String.Table.of_alist_multi
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (file, entries) ->
      Value.cons
        (Stdlib.Filename.concat in_dir file |> Value.of_utf8_bytes)
        (Value.list (entries |> List.map ~f:Load_history.Entry.to_value)))
    |> Value.list
  in
  Current_buffer.set_value
    load_history
    (append addition (Current_buffer.value_exn load_history))
;;
