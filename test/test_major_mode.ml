open! Core_kernel
open! Import
open! Major_mode

let test_mode = "test-major-mode" |> Symbol.intern

type Major_mode.Name.t += Major_mode

let t =
  define_derived_mode
    [%here]
    Major_mode
    ~change_command:test_mode
    ~docstring:"docstring"
    ~initialize:(fun () -> print_s [%message "initialized"])
    ~mode_line:"<test-mode mode line>"
;;

let%expect_test "duplicate [define_derived_mode]" =
  show_raise ~hide_positions:true (fun () ->
    define_derived_mode
      [%here]
      Major_mode
      ~change_command:test_mode
      ~docstring:"docstring"
      ~initialize:(fun () -> print_s [%message "initialized"])
      ~mode_line:"<test-mode mode line>");
  [%expect
    {|
    (raised (
      "Already associated with a name."
      (change_command test-major-mode)
      (here app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL)
      (previous_def app/emacs/lib/ecaml/test/test_major_mode.ml:LINE:COL))) |}]
;;

let%expect_test "duplicate name is NOT caught" =
  show_raise (fun () ->
    define_derived_mode
      [%here]
      Major_mode
      ~change_command:("other-mode" |> Symbol.intern)
      ~docstring:""
      ~initialize:Fn.id
      ~mode_line:"");
  [%expect {| "did not raise" |}]
;;

let%expect_test "[define_derived_mode]" =
  let show_major_mode () = print_s [%sexp (Current_buffer.major_mode () : t)] in
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    show_major_mode ();
    [%expect
      {|
      ((change_command fundamental-mode)
       (keymap_var (fundamental-mode-map keymap))
       (name <opaque>)
       (syntax_table_var (fundamental-mode-syntax-table syntax-table))) |}];
    Current_buffer.change_major_mode t;
    [%expect {|
      initialized |}];
    show_major_mode ();
    [%expect
      {|
      ((change_command test-major-mode)
       (keymap_var (test-major-mode-map keymap))
       (name <opaque>)
       (syntax_table_var (test-major-mode-syntax-table syntax-table))) |}])
;;

let%expect_test "[keymap]" =
  print_s [%sexp (keymap t : Keymap.t)];
  [%expect {|
    (keymap) |}]
;;

let show_syntax_table t = Test_syntax_table.show (syntax_table t)

let%expect_test "[syntax_table]" =
  show_syntax_table t;
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
  show_syntax_table prog;
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
  show_syntax_table special;
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
  show_syntax_table text;
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
