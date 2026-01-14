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
     : Major_mode.t);
  update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
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
  update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
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
  update_emacs_with_entries ~chop_prefix:"app/emacs/" ~in_dir:"<dir>";
  show_defining_file (fun sym ->
    (* defstruct doesn't define anything for the base symbol. Also ignore random internal
       stuff that [cl-defstruct] defines. *)
    String.is_substring sym ~substring:(Symbol.name base_symbol)
    && (not (String.equal sym (Symbol.name base_symbol)))
    && (not (String.is_prefix sym ~prefix:"--cl-block-"))
    && (not (String.is_suffix sym ~suffix:"--cmacro"))
    && not (String.is_suffix sym ~suffix:"-struct-tags"));
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
