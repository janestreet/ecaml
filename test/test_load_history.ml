open! Core
open! Async_kernel
open! Import
open! Load_history

let show_defining_file ?cr filter_symbol_name =
  let symbols_defined = ref Reversed_list.[] in
  Obarray.iter Obarray.standard ~f:(fun symbol ->
    if filter_symbol_name (Symbol.name symbol)
    then symbols_defined := symbol :: !symbols_defined);
  let definition_by_symbol =
    Reversed_list.rev !symbols_defined
    |> List.map ~f:(fun symbol -> Symbol.name symbol, defining_file symbol)
    |> String.Map.of_alist_exn
  in
  Map.iteri definition_by_symbol ~f:(fun ~key:symbol ~data:defining_file ->
    match defining_file with
    | None -> print_cr ?cr [%message "No load-history entry for symbol" (symbol : string)]
    | Some defining_file -> print_s [%sexp (symbol : string), (defining_file : string)])
;;

let%expect_test "[defcustom], [defvar], [defun], [update_emacs_with_entries], \
                 [defining_file]"
  =
  let custom = "custom" |> Symbol.intern in
  let var = "var" |> Symbol.intern in
  let fun_ = "fun" |> Symbol.intern in
  ignore
    (defcustom
       custom
       [%here]
       ~docstring:"custom docstring"
       ~group:("custom-group" |> Customization.Group.of_string)
       ~type_:Value.Type.bool
       ~customization_type:Boolean
       ~standard_value:false
       ()
     : _ Customization.t);
  ignore
    (defvar var [%here] ~docstring:"foo" ~type_:Value.Type.bool ~initial_value:false ()
     : _ Var.t);
  defun_nullary_nil fun_ [%here] ~docstring:"<docstring>" Fn.id;
  Find_function.update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  let show_defining_file symbol =
    print_s [%sexp (defining_file symbol : string option)]
  in
  show_defining_file custom;
  [%expect {| (<dir>/lib/ecaml/test/test_load_history.ml) |}];
  show_defining_file var;
  [%expect {| (<dir>/lib/ecaml/test/test_load_history.ml) |}];
  show_defining_file fun_;
  [%expect {| (<dir>/lib/ecaml/test/test_load_history.ml) |}];
  return ()
;;

let%expect_test "[find-function] and [find-variable] jump to the Ecaml definition" =
  let fun_ = "test-load-history-find-function-target" |> Symbol.intern in
  let var = "test-load-history-find-variable-target" |> Symbol.intern in
  let mode = "test-load-history-find-mode" |> Symbol.intern in
  (* The box-drawing characters below are multibyte, so the byte and character offsets of
     the definitions that follow differ; [Find_function] must treat [pos_cnum] as a byte
     offset. [find-function-search-for-symbol] moves to the beginning of the line after
     finding the definition, so the difference has to exceed a line's length to be
     visible.
     ────────────────────────────────────────────────────────────────────────────── *)
  defun_nullary_nil fun_ [%here] ~docstring:"<docstring>" Fn.id;
  ignore
    (defvar var [%here] ~docstring:"foo" ~type_:Value.Type.bool ~initial_value:false ()
     : _ Var.t);
  ignore
    (define_derived_mode
       mode
       [%here]
       ~docstring:"derived major mode docstring"
       ~mode_line:"test-load-history-find"
       ()
     : Major_mode.t);
  (* Point [load-history] at the copy of this file in the build sandbox, so that
     [find-function] can actually visit it. *)
  Find_function.update_emacs_with_entries
    ~chop_prefix:"app/emacs/"
    ~in_dir:(Core_unix.getcwd () ^ "/../../..");
  let find_function = Funcall.Wrap.("find-function" <: Symbol.t @-> return nil) in
  let find_variable = Funcall.Wrap.("find-variable" <: Symbol.t @-> return nil) in
  let test find symbol =
    Current_buffer.set_temporarily_to_temp_buffer (fun () ->
      Echo_area.inhibit_messages (fun () -> find symbol);
      let buffer = Current_buffer.get () in
      let line_and_column = Point.get_line_and_column () in
      let line =
        Current_buffer.contents
          ~start:(Point.beginning_of_line_position ())
          ~end_:(Point.end_of_line_position ())
          ()
      in
      print_s
        [%sexp { buffer : Buffer.t; line_and_column : Line_and_column.t; line : Text.t }])
  in
  test find_function fun_;
  [%expect
    {|
    ((buffer "#<buffer test_load_history.ml>")
     (line_and_column (
       (line   66)
       (column 0)))
     (line "  defun_nullary_nil fun_ [%here] ~docstring:\"<docstring>\" Fn.id;"))
    |}];
  test find_variable var;
  [%expect
    {|
    ((buffer "#<buffer test_load_history.ml>")
     (line_and_column (
       (line   68)
       (column 0)))
     (line
      "    (defvar var [%here] ~docstring:\"foo\" ~type_:Value.Type.bool ~initial_value:false ()"))
    |}];
  (* [define-derived-mode] gives the keymap and syntax table (but not the hook) a
     [definition-name] property naming the mode, which [find-function-search-for-symbol]
     follows before looking for the definition. *)
  List.iter [ "map"; "syntax-table"; "hook" ] ~f:(fun suffix ->
    test find_variable ([%string "%{Symbol.name mode}-%{suffix}"] |> Symbol.intern));
  [%expect
    {|
    ((buffer "#<buffer test_load_history.ml>")
     (line_and_column (
       (line   73)
       (column 0)))
     (line "       [%here]"))
    ((buffer "#<buffer test_load_history.ml>")
     (line_and_column (
       (line   73)
       (column 0)))
     (line "       [%here]"))
    ((buffer "#<buffer test_load_history.ml>")
     (line_and_column (
       (line   73)
       (column 0)))
     (line "       [%here]"))
    |}];
  return ()
;;

let%expect_test "[define_derived_mode]" =
  let base_symbol = "test-load-history-major-mode" |> Symbol.intern in
  ignore
    (define_derived_mode
       base_symbol
       [%here]
       ~docstring:"derived major mode docstring"
       ~mode_line:"test-load-history"
       ()
     : Major_mode.t);
  Find_function.update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  show_defining_file (String.is_prefix ~prefix:(Symbol.name base_symbol));
  [%expect
    {|
    (test-load-history-major-mode <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-major-mode-abbrev-table
     <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-major-mode-hook <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-major-mode-map <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-major-mode-syntax-table
     <dir>/lib/ecaml/test/test_load_history.ml)
    |}];
  return ()
;;

let%expect_test "[define_minor_mode]" =
  let base_symbol = "test-load-history-minor-mode" |> Symbol.intern in
  ignore
    (define_minor_mode
       base_symbol
       [%here]
       ~global:None
       ~docstring:"minor mode docstring"
       ()
     : Minor_mode.t);
  Find_function.update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  show_defining_file (String.is_prefix ~prefix:(Symbol.name base_symbol));
  [%expect
    {|
    (test-load-history-minor-mode <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-minor-mode-hook <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-minor-mode-map <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-minor-mode-off-hook
     <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-minor-mode-on-hook
     <dir>/lib/ecaml/test/test_load_history.ml)
    |}];
  return ()
;;

let%expect_test "[defstruct]" =
  let base_symbol = "test-load-history-struct" |> Symbol.intern in
  ignore
    (Defstruct.defstruct
       ~here:[%here]
       ~name:(Symbol.name base_symbol)
       ~doc:"cl-defstruct docstring"
       Defstruct.Field.
         [ field "host" Host_and_port.host string; field "port" Host_and_port.port int ]
     : Host_and_port.t Defstruct.t);
  Find_function.update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  show_defining_file (fun sym ->
    (* defstruct doesn't define anything for the base symbol. Also ignore random internal
       stuff that [cl-defstruct] defines. *)
    String.is_substring sym ~substring:(Symbol.name base_symbol)
    && (not (String.equal sym (Symbol.name base_symbol)))
    && (not (String.is_prefix sym ~prefix:"--cl-block-"))
    && (not (String.is_suffix sym ~suffix:"--cmacro"))
    && (not (String.is_suffix sym ~suffix:"-struct-tags"))
    && not (String.is_suffix sym ~suffix:"--inliner"));
  [%expect
    {|
    (copy-test-load-history-struct <dir>/lib/ecaml/test/test_load_history.ml)
    (make-test-load-history-struct <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-struct-host <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-struct-p <dir>/lib/ecaml/test/test_load_history.ml)
    (test-load-history-struct-port <dir>/lib/ecaml/test/test_load_history.ml)
    |}];
  return ()
;;
