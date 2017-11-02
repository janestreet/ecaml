open! Core_kernel
open! Import
open! Point

let show () = print_s [%message "" ~point:(get () : Position.t)]

let%expect_test "[get]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    show ();
    [%expect {|
      (point 1) |}];
    print_s [%sexp (Current_buffer.get () : Buffer.t)];
    insert "foo";
    show ();
    [%expect {|
      "#<buffer *temp-buffer*>"
      (point 4) |}]);
;;

let%expect_test "[min], [max]" =
  let show_min_max () =
    print_s [%message
      ""
        ~min:(min () : Position.t)
        ~max:(max () : Position.t)] in
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    show_min_max ();
    [%expect {|
      ((min 1)
       (max 1)) |}];
    insert "abc";
    show_min_max ();
    [%expect {|
      ((min 1)
       (max 4)) |}]);
;;

let%expect_test "[backward_char_exn] raise" =
  show_raise (fun () ->
    Current_buffer.set_temporarily_to_temp_buffer (fun () -> backward_char_exn 1));
  [%expect {|
    (raised beginning-of-buffer) |}];
;;

let%expect_test "[forward_char_exn] raise" =
  show_raise (fun () ->
    Current_buffer.set_temporarily_to_temp_buffer (fun () -> forward_char_exn 1));
  [%expect {|
    (raised end-of-buffer) |}];
;;

let%expect_test "[forward_char_exn], [backward_char_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    forward_char_exn 0;
    backward_char_exn 0;
    insert "foo";
    show ();
    [%expect {|
      (point 4) |}];
    backward_char_exn 1;
    show ();
    [%expect {|
      (point 3) |}];
    backward_char_exn 2;
    show ();
    [%expect {|
      (point 1) |}];
    forward_char_exn 1;
    show ();
    [%expect {|
      (point 2) |}];
    forward_char_exn 2;
    show ();
    [%expect {|
      (point 4) |}]);
;;

let%expect_test "[backward_line], [forward_line], [line_number]" =
  let show_line_num () =
    print_s [%message "" ~line_number:(line_number () : int)] in
  let backward_line n = backward_line n; show_line_num () in
  let forward_line  n = forward_line  n; show_line_num () in
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    backward_line 1;
    [%expect {|
      (line_number 1) |}];
    forward_line 1;
    [%expect {|
      (line_number 1) |}];
    for _ = 1 to 10 do
      insert "foo\n";
    done;
    show_line_num ();
    [%expect {|
      (line_number 11) |}];
    forward_line (-1);
    [%expect {|
      (line_number 10) |}];
    forward_line 2;
    [%expect {|
      (line_number 11) |}];
    backward_line 8;
    [%expect {|
      (line_number 3) |}];
    backward_line 100;
    [%expect {|
      (line_number 1) |}]);
;;

let%expect_test "[forward_sexp_exn], [backward_sexp_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "(a b c) d (ef)";
    let backward_sexp_exn i = backward_sexp_exn i; show () in
    let forward_sexp_exn  i = forward_sexp_exn  i; show () in
    backward_sexp_exn 1;
    [%expect {|
      (point 11) |}];
    backward_sexp_exn 2;
    [%expect {|
      (point 1) |}];
    forward_sexp_exn 1;
    [%expect {|
      (point 8) |}];
    forward_sexp_exn 2;
    [%expect {|
      (point 15) |}]);
;;

let%expect_test "[forward_sexp_exn], [backward_sexp_exn] raise" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "a b c) d (ef)";
    let backward_sexp_exn i = backward_sexp_exn i; show () in
    let forward_sexp_exn  i = forward_sexp_exn  i; show () in
    backward_sexp_exn 2;
    [%expect {|
      (point 8) |}];
    show_raise (fun () -> backward_sexp_exn 1);
    [%expect {|
      (raised (scan-error ("Unbalanced parentheses" 6 1))) |}];
    goto_char (Point.min ());
    forward_sexp_exn 3;
    [%expect {|
      (point 6) |}];
    show_raise (fun () -> forward_sexp_exn 1);
    [%expect {|
      (raised (scan-error ("Containing expression ends prematurely" 6 7))) |}]);
;;

let%expect_test "[forward_word], [backward_word]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "word1 word2 word3";
    let backward_word i = backward_word i; show () in
    let forward_word  i = forward_word  i; show () in
    backward_word 1;
    [%expect {|
      (point 13) |}];
    backward_word 2;
    [%expect {|
      (point 1) |}];
    forward_word 1;
    [%expect {|
      (point 6) |}];
    forward_word 2;
    [%expect {|
      (point 18) |}]);
;;

let%expect_test "[kill_word]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "word1 word2 word3";
    let kill_word i =
      kill_word i;
      print_s [%sexp (Current_buffer.contents () : Text.t)] in
    goto_char (Point.min ());
    kill_word 1;
    [%expect {|
      " word2 word3" |}];
    kill_word 2;
    [%expect {|
      "" |}]);
;;

let%expect_test "[column_number], [goto_column]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "foo";
    show_raise (fun () -> goto_column (-1));
    [%expect {| (raised (wrong-type-argument (wholenump -1))) |}];
    for i = 0 to 4 do
      goto_column i;
      print_s [%sexp (column_number () : int)];
    done;
    [%expect {|
      0
      1
      2
      3
      3 |}]);
;;

let%expect_test "[insert_file_contents_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert_file_contents_exn "test_point.ml";
    print_s [%sexp (Current_buffer.contents () ~end_:(11 |> Position.of_int_exn)
                    : Text.t)]);
  [%expect {|
    "open! Core" |}];
;;

let%expect_test "[insert_file_contents_exn] raise" =
  show_raise (fun () ->
    Current_buffer.set_temporarily_to_temp_buffer (fun () ->
      insert_file_contents_exn "/zzz";
      print_s [%sexp (Current_buffer.contents () ~end_:(11 |> Position.of_int_exn)
                      : Text.t)]));
  [%expect {|
    (raised (file-error ("Opening input file" "No such file or directory" /zzz))) |}];
;;

let%expect_test "[marker_at*]" =
  let show marker = print_s [%sexp (marker : Marker.t)] in
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    Point.goto_char (2 |> Position.of_int_exn);
    show (marker_at ());
    [%expect {|
      "#<marker at 2 in *temp-buffer*>" |}];
    show (marker_at_min ());
    [%expect {|
      "#<marker at 1 in *temp-buffer*>" |}];
    show (marker_at_max ());
    [%expect {|
      "#<marker at 4 in *temp-buffer*>" |}]);
;;

let%expect_test "[search_{back,for}ward]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abc";
    goto_char (min ());
    show ();
    [%expect {|
      (point 1) |}];
    print_s [%sexp (search_forward "b" : bool)];
    [%expect {|
      true |}];
    show ();
    [%expect {|
      (point 3) |}];
    print_s [%sexp (search_forward "z" : bool)];
    [%expect {|
      false |}];
    show ();
    [%expect {|
      (point 3) |}];
    print_s [%sexp (search_backward "a" : bool)];
    [%expect {|
      true |}];
    show ();
    [%expect {|
      (point 1) |}];
    print_s [%sexp (search_backward "z" : bool)];
    [%expect {|
      false |}];
    show ();
    [%expect {|
      (point 1) |}]);
;;

let%expect_test "[search_{back,for}ward_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abc";
    goto_char (min ());
    show ();
    [%expect {|
      (point 1) |}];
    search_forward_exn "b";
    show ();
    [%expect {|
      (point 3) |}];
    show_raise (fun () -> search_forward_exn "z");
    [%expect {|
      (raised ("string not found" (string z))) |}];
    show ();
    [%expect {|
      (point 3) |}];
    search_backward_exn "a";
    show ();
    [%expect {|
      (point 1) |}];
    show_raise (fun () -> search_backward_exn "z");
    [%expect {|
      (raised ("string not found" (string z))) |}];
    show ();
    [%expect {|
      (point 1) |}]);
;;

let%expect_test "[search_forward]_exn] and [Regexp.Last_match]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abcdef";
    goto_char (min ());
    show_raise (fun () -> search_forward_exn "zz" ~update_last_match:true);
    [%expect {|
      (raised ("string not found" (string zz))) |}];
    show_last_match ();
    [%expect {|
      ((text  (Error "Prior [Regexp] match did not match"))
       (start (Error "Prior [Regexp] match did not match"))
       (end_  (Error "Prior [Regexp] match did not match"))) |}];
    search_forward_exn "bc" ~update_last_match:true;
    show_last_match ();
    [%expect {|
      ((text  (Ok bc))
       (start (Ok 2))
       (end_  (Ok 4))) |}]);
;;

let%expect_test "[search_{back,for}ward_regexp]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abc";
    goto_char (min ());
    show ();
    [%expect {|
      (point 1) |}];
    print_s [%sexp (search_forward_regexp ("b" |> Regexp.quote) : bool)];
    [%expect {|
      true |}];
    show ();
    [%expect {|
      (point 3) |}];
    print_s [%sexp (search_forward_regexp ("z" |> Regexp.quote) : bool)];
    [%expect {|
      false |}];
    show ();
    [%expect {|
      (point 3) |}];
    print_s [%sexp (search_backward_regexp ("a" |> Regexp.quote) : bool)];
    [%expect {|
      true |}];
    show ();
    [%expect {|
      (point 1) |}];
    print_s [%sexp (search_backward_regexp ("z" |> Regexp.quote) : bool)];
    [%expect {|
      false |}];
    show ();
    [%expect {|
      (point 1) |}]);
;;

let%expect_test "[search_{back,for}ward_regexp_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abc";
    goto_char (min ());
    show ();
    [%expect {|
      (point 1) |}];
    search_forward_regexp_exn ("b" |> Regexp.quote);
    show ();
    [%expect {|
      (point 3) |}];
    show_raise (fun () -> search_forward_regexp_exn ("z" |> Regexp.quote));
    [%expect {|
      (raised ("regexp not found" (regexp z))) |}];
    show ();
    [%expect {|
      (point 3) |}];
    search_backward_regexp_exn ("a" |> Regexp.quote);
    show ();
    [%expect {|
      (point 1) |}];
    show_raise (fun () -> search_backward_regexp_exn ("z" |> Regexp.quote));
    [%expect {|
      (raised ("regexp not found" (regexp z))) |}];
    show ();
    [%expect {|
      (point 1) |}]);
;;

let%expect_test "[search_forward_regexp] and [Regexp.Last_match]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abc";
    goto_char (min ());
    print_s [%sexp (search_forward_regexp ("z" |> Regexp.of_pattern)
                      ~update_last_match:true
                    : bool)];
    [%expect {|
      false |}];
    show_last_match ();
    [%expect {|
      ((text  (Error "Prior [Regexp] match did not match"))
       (start (Error "Prior [Regexp] match did not match"))
       (end_  (Error "Prior [Regexp] match did not match"))) |}];
    print_s [%sexp (search_forward_regexp ({|\(b\)|} |> Regexp.of_pattern)
                      ~update_last_match:true
                    : bool)];
    [%expect {|
      true |}];
    show_last_match ();
    [%expect {|
      ((text  (Ok b))
       (start (Ok 2))
       (end_  (Ok 3))) |}];
    show_last_match () ~subexp:1;
    [%expect {|
      ((text  (Ok b))
       (start (Ok 2))
       (end_  (Ok 3))) |}]);
;;

let%expect_test "[Regexp.Last_match] in wrong buffer" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abc";
    search_backward_exn "b" ~update_last_match:true;
    show_last_match ();
    [%expect {|
      ((text  (Ok b))
       (start (Ok 2))
       (end_  (Ok 3))) |}]);
  show_last_match ();
  [%expect {|
    ((text (
       Error (
         "[Regexp.Last_match.text_exn] called in wrong buffer"
         (current  "#<buffer *scratch*>")
         (expected "#<killed buffer>"))))
     (start (Ok 2))
     (end_  (Ok 3))) |}];
;;

let%expect_test "[looking_at]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abc";
    goto_char (min ());
    let looking_at s = print_s [%sexp (looking_at (s |> Regexp.quote) : bool)] in
    looking_at "a";
    [%expect {|
      true |}];
    looking_at "b";
    [%expect {|
      false |}]);
;;

let%expect_test "[looking_at ~update_last_match:true]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert "abc";
    goto_char (min ());
    let looking_at s =
      print_s [%sexp (looking_at (s |> Regexp.quote) ~update_last_match:true : bool)];
      show_last_match () in
    looking_at "a";
    [%expect {|
      true
      ((text  (Ok a))
       (start (Ok 1))
       (end_  (Ok 2))) |}];
    looking_at "b";
    [%expect {|
      false
      ((text  (Error "Prior [Regexp] match did not match"))
       (start (Error "Prior [Regexp] match did not match"))
       (end_  (Error "Prior [Regexp] match did not match"))) |}]);
;;

let%expect_test "[goto_line]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    insert {|
a
bc
def
|};
    let test line =
      goto_line line;
      print_s [%sexp (get () : Position.t)] in
    test 0;
    [%expect {|
      1 |}];
    test 1;
    [%expect {|
      1 |}];
    test 2;
    [%expect {|
      2 |}];
    test 3;
    [%expect {|
      4 |}];
    test 4;
    [%expect {|
      7 |}])
;;
