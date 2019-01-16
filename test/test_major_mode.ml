open! Core_kernel
open! Import
open! Major_mode

let test_mode = "test-major-mode" |> Symbol.intern

module M =
  (val define_derived_mode
         test_mode
         [%here]
         ~docstring:"docstring"
         ~mode_line:"<test-mode mode line>"
         ~initialize:(fun () -> print_s [%message "initialized"])
         ())

let%expect_test "duplicate [define_derived_mode]" =
  show_raise ~hide_positions:true (fun () ->
    define_derived_mode
      test_mode
      [%here]
      ~docstring:"docstring"
      ~mode_line:"<test-mode mode line>"
      ~initialize:(fun () -> print_s [%message "initialized"])
      ());
  [%expect
    {|
    (raised (
      "Already associated with a name."
      (symbol test-major-mode)
      (wrapped_at app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL)
      (previous_def (
        (wrapped_at app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL)
        (symbol test-major-mode)
        (keymap_var (test-major-mode-map keymap))
        (name <opaque>)
        (syntax_table_var (test-major-mode-syntax-table syntax-table)))))) |}]
;;

let%expect_test "duplicate name is NOT caught" =
  show_raise (fun () ->
    define_derived_mode
      ("other-mode" |> Symbol.intern)
      [%here]
      ~docstring:""
      ~mode_line:""
      ());
  [%expect {| "did not raise" |}]
;;

let%expect_test "[define_derived_mode]" =
  let show_major_mode () =
    print_s [%sexp (Current_buffer.major_mode () : t)] ~hide_positions:true
  in
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    show_major_mode ();
    [%expect
      {|
      ((wrapped_at app/emacs/lib/ecaml/src/major_mode.ml:LINE:COL)
       (symbol fundamental-mode)
       (keymap_var (fundamental-mode-map keymap))
       (name <opaque>)
       (syntax_table_var (fundamental-mode-syntax-table syntax-table))) |}];
    Current_buffer.change_major_mode M.major_mode;
    [%expect {|
      initialized |}];
    show_major_mode ();
    [%expect
      {|
      ((wrapped_at app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL)
       (symbol test-major-mode)
       (keymap_var (test-major-mode-map keymap))
       (name <opaque>)
       (syntax_table_var (test-major-mode-syntax-table syntax-table))) |}])
;;

let%expect_test "[keymap]" =
  print_s [%sexp (keymap M.major_mode : Keymap.t)];
  [%expect {|
    (keymap) |}]
;;

let show_syntax_table t = Test_syntax_table.show (syntax_table t)

let%expect_test "[syntax_table]" =
  show_syntax_table M.major_mode;
  [%expect
    {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}]
;;

let%expect_test "[prog]" =
  show_syntax_table Prog.major_mode;
  [%expect
    {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}]
;;

let%expect_test "[special]" =
  show_syntax_table Special.major_mode;
  [%expect
    {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}]
;;

let%expect_test "[text]" =
  show_syntax_table Text.major_mode;
  [%expect
    {|
    ((Close_paren         ")]}")
     (Open_paren          "([{")
     (Punctuation         "!\"#,.:;?@\\^`~")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}]
;;

let%expect_test "[is_derived]" =
  print_s [%sexp (is_derived Text.major_mode ~from:Special.major_mode : bool)];
  [%expect {| false |}];
  let module M =
    (val define_derived_mode
           ("texting-mode" |> Symbol.intern)
           [%here]
           ~docstring:""
           ~mode_line:"texting"
           ~parent:Text.major_mode
           ())
  in
  print_s [%sexp (is_derived M.major_mode ~from:Text.major_mode : bool)];
  [%expect {| true |}]
;;
