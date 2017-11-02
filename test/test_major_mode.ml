open! Core_kernel
open! Import
open! Major_mode

let test_mode = "test-mode" |> Symbol.intern

let t =
  define_derived_mode [%here]
    ~change_command:test_mode
    ~docstring:"docstring"
    ~initialize:(fun () -> print_s [%message "initialized"])
    ~mode_line:"<test-mode mode line>"
;;

let%expect_test "[define_derived_mode]" =
  let show_major_mode () =
    print_s [%sexp (Current_buffer.major_mode () : t)] in
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    show_major_mode ();
    [%expect {|
      ((change_command fundamental-mode)
       (keymap_var (fundamental-mode-map keymap))
       (syntax_table_var (fundamental-mode-syntax-table syntax-table))) |}];
    Current_buffer.change_major_mode t;
    [%expect {|
      initialized |}];
    show_major_mode ();
    [%expect {|
      ((change_command test-mode)
       (keymap_var       (test-mode-map          keymap))
       (syntax_table_var (test-mode-syntax-table syntax-table))) |}])
;;

let%expect_test "[keymap]" =
  print_s [%sexp (keymap t : Keymap.t)];
  [%expect {|
    (keymap) |}];
;;

let show_syntax_table t = Test_syntax_table.show (syntax_table t)

let%expect_test "[syntax_table]" =
  show_syntax_table t;
  [%expect {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}];
;;

let%expect_test "[prog]" =
  show_syntax_table prog;
  [%expect {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}];
;;

let%expect_test "[special]" =
  show_syntax_table special;
  [%expect {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}];
;;

let%expect_test "[text]" =
  show_syntax_table text;
  [%expect {|
    ((Close_paren         ")]}")
     (Open_paren          "([{")
     (Punctuation         "!\"#,.:;?@\\^`~")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}];
;;
