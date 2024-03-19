open! Core
open! Async_kernel
open! Import
open! Load_history

let show_defining_file ?cr symbol_prefix =
  let symbols_defined = ref Reversed_list.[] in
  Obarray.iter Obarray.standard ~f:(fun symbol ->
    if String.is_prefix (Symbol.name symbol) ~prefix:(Symbol.name symbol_prefix)
    then symbols_defined := symbol :: !symbols_defined);
  let definition_by_symbol =
    Reversed_list.rev !symbols_defined
    |> List.map ~f:(fun symbol -> Symbol.name symbol, defining_file symbol)
    |> String.Map.of_alist_exn
  in
  Map.iteri definition_by_symbol ~f:(fun ~key:symbol ~data:defining_file ->
    match defining_file with
    | None ->
      print_cr ?cr [%here] [%message "No load-history entry for symbol" (symbol : string)]
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
  update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
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

let%expect_test "[define_derived_mode]" =
  let base_symbol = "test-load-history-major-mode" |> Symbol.intern in
  ignore
    (define_derived_mode
       base_symbol
       [%here]
       ~docstring:"derived major mode docstring"
       ~mode_line:"test-load-history"
       ()
      : (module Major_mode.S));
  update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  show_defining_file base_symbol;
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
       ~global:false
       ~docstring:"minor mode docstring"
       ()
      : Minor_mode.t);
  update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  show_defining_file base_symbol;
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
