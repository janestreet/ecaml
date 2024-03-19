open! Core
open! Async_kernel
open! Import
open! Point

let show () = print_s [%message "" ~point:(get () : Position.t)]

let%expect_test "[get]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    show ();
    [%expect {| (point 1) |}];
    print_s [%sexp (Current_buffer.get () : Buffer.t)];
    insert "foo";
    show ();
    [%expect {|
      "#<buffer  *temp*>"
      (point 4)
      |}]);
  return ()
;;

let%expect_test "[min], [max]" =
  let show_min_max () =
    print_s [%message "" ~min:(min () : Position.t) ~max:(max () : Position.t)]
  in
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    show_min_max ();
    [%expect {|
      ((min 1)
       (max 1))
      |}];
    insert "abc";
    show_min_max ();
    [%expect {|
      ((min 1)
       (max 4))
      |}]);
  return ()
;;

let%expect_test "[backward_char_exn] raise" =
  show_raise (fun () ->
    Current_buffer.set_temporarily_to_temp_buffer Sync (fun () -> backward_char_exn 1));
  [%expect {| (raised beginning-of-buffer) |}];
  return ()
;;

let%expect_test "[forward_char_exn] raise" =
  show_raise (fun () ->
    Current_buffer.set_temporarily_to_temp_buffer Sync (fun () -> forward_char_exn 1));
  [%expect {| (raised end-of-buffer) |}];
  return ()
;;

let%expect_test "[forward_char_exn], [backward_char_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    forward_char_exn 0;
    backward_char_exn 0;
    insert "foo";
    show ();
    [%expect {| (point 4) |}];
    backward_char_exn 1;
    show ();
    [%expect {| (point 3) |}];
    backward_char_exn 2;
    show ();
    [%expect {| (point 1) |}];
    forward_char_exn 1;
    show ();
    [%expect {| (point 2) |}];
    forward_char_exn 2;
    show ();
    [%expect {| (point 4) |}]);
  return ()
;;

let%expect_test "[backward_line], [forward_line], [line_number]" =
  let show_line_num () = print_s [%message "" ~line_number:(line_number () : int)] in
  let backward_line n =
    backward_line n;
    show_line_num ()
  in
  let forward_line n =
    forward_line n;
    show_line_num ()
  in
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    backward_line 1;
    [%expect {| (line_number 1) |}];
    forward_line 1;
    [%expect {| (line_number 1) |}];
    for _ = 1 to 10 do
      insert "foo\n"
    done;
    show_line_num ();
    [%expect {| (line_number 11) |}];
    forward_line (-1);
    [%expect {| (line_number 10) |}];
    forward_line 2;
    [%expect {| (line_number 11) |}];
    backward_line 8;
    [%expect {| (line_number 3) |}];
    backward_line 100;
    [%expect {| (line_number 1) |}]);
  return ()
;;

let%expect_test "[forward_sexp_exn], [backward_sexp_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "(a b c) d (ef)";
    let backward_sexp_exn i =
      backward_sexp_exn i;
      show ()
    in
    let forward_sexp_exn i =
      forward_sexp_exn i;
      show ()
    in
    backward_sexp_exn 1;
    [%expect {| (point 11) |}];
    backward_sexp_exn 2;
    [%expect {| (point 1) |}];
    forward_sexp_exn 1;
    [%expect {| (point 8) |}];
    forward_sexp_exn 2;
    [%expect {| (point 15) |}]);
  return ()
;;

let%expect_test "[forward_sexp_exn], [backward_sexp_exn] raise" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "a b c) d (ef)";
    let backward_sexp_exn i =
      backward_sexp_exn i;
      show ()
    in
    let forward_sexp_exn i =
      forward_sexp_exn i;
      show ()
    in
    backward_sexp_exn 2;
    [%expect {| (point 8) |}];
    show_raise (fun () -> backward_sexp_exn 1);
    [%expect {| (raised (scan-error ("Unbalanced parentheses" 6 1))) |}];
    goto_char (Point.min ());
    forward_sexp_exn 3;
    [%expect {| (point 6) |}];
    show_raise (fun () -> forward_sexp_exn 1);
    [%expect {| (raised (scan-error ("Containing expression ends prematurely" 6 7))) |}]);
  return ()
;;

let%expect_test "[forward_word], [backward_word]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "word1 word2 word3";
    let backward_word i =
      backward_word i;
      show ()
    in
    let forward_word i =
      forward_word i;
      show ()
    in
    backward_word 1;
    [%expect {| (point 13) |}];
    backward_word 2;
    [%expect {| (point 1) |}];
    forward_word 1;
    [%expect {| (point 6) |}];
    forward_word 2;
    [%expect {| (point 18) |}]);
  return ()
;;

let%expect_test "[kill_word]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "word1 word2 word3";
    let kill_word i =
      kill_word i;
      print_s [%sexp (Current_buffer.contents () : Text.t)]
    in
    goto_char (Point.min ());
    kill_word 1;
    [%expect {| " word2 word3" |}];
    kill_word 2;
    [%expect {| "" |}]);
  return ()
;;

let%expect_test "[column_number], [goto_column]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "foo";
    show_raise (fun () -> goto_column (-1));
    [%expect {| (raised (wrong-type-argument (wholenump -1))) |}];
    for i = 0 to 4 do
      goto_column i;
      print_s [%sexp (column_number () : int)]
    done;
    [%expect {|
      0
      1
      2
      3
      3
      |}]);
  return ()
;;

let%expect_test "[insert_file_contents_exn ~replace:false]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "foo";
    insert_file_contents_exn ~replace:false "test_point.ml";
    print_s
      [%sexp (Current_buffer.contents () ~end_:(11 |> Position.of_int_exn) : Text.t)]);
  [%expect {| "fooopen! C" |}];
  return ()
;;

let%expect_test "[insert_file_contents_exn ~replace:true]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "foo";
    insert_file_contents_exn "test_point.ml" ~replace:true;
    print_s
      [%sexp (Current_buffer.contents () ~end_:(11 |> Position.of_int_exn) : Text.t)]);
  [%expect {| "open! Core" |}];
  return ()
;;

let%expect_test "[insert_file_contents_exn] raise" =
  show_raise (fun () ->
    Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
      insert_file_contents_exn "/zzz";
      print_s
        [%sexp (Current_buffer.contents () ~end_:(11 |> Position.of_int_exn) : Text.t)]));
  [%expect
    {| (raised (file-missing ("Opening input file" "No such file or directory" /zzz))) |}];
  return ()
;;

let%expect_test "[marker_at*]" =
  let show marker = print_s [%sexp (marker : Marker.t)] in
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "foo";
    Point.goto_char (2 |> Position.of_int_exn);
    show (marker_at ());
    [%expect {| "#<marker at 2 in  *temp*>" |}];
    show (marker_at_min ());
    [%expect {| "#<marker at 1 in  *temp*>" |}];
    show (marker_at_max ());
    [%expect {| "#<marker at 4 in  *temp*>" |}]);
  return ()
;;

let%expect_test "[search_{back,for}ward]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abc";
    goto_char (min ());
    show ();
    [%expect {| (point 1) |}];
    print_s [%sexp (search_forward "b" : bool)];
    [%expect {| true |}];
    show ();
    [%expect {| (point 3) |}];
    print_s [%sexp (search_forward "z" : bool)];
    [%expect {| false |}];
    show ();
    [%expect {| (point 3) |}];
    print_s [%sexp (search_backward "a" : bool)];
    [%expect {| true |}];
    show ();
    [%expect {| (point 1) |}];
    print_s [%sexp (search_backward "z" : bool)];
    [%expect {| false |}];
    show ();
    [%expect {| (point 1) |}]);
  return ()
;;

let%expect_test "[search_{back,for}ward_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abc";
    goto_char (min ());
    show ();
    [%expect {| (point 1) |}];
    search_forward_exn "b";
    show ();
    [%expect {| (point 3) |}];
    show_raise (fun () -> search_forward_exn "z");
    [%expect {| (raised ("string not found" (string z))) |}];
    show ();
    [%expect {| (point 3) |}];
    search_backward_exn "a";
    show ();
    [%expect {| (point 1) |}];
    show_raise (fun () -> search_backward_exn "z");
    [%expect {| (raised ("string not found" (string z))) |}];
    show ();
    [%expect {| (point 1) |}]);
  return ()
;;

let%expect_test "[search_forward]_exn] and [Regexp.Last_match]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abcdef";
    goto_char (min ());
    show_raise (fun () -> search_forward_exn "zz" ~update_last_match:true);
    [%expect {| (raised ("string not found" (string zz))) |}];
    show_last_match ();
    [%expect
      {|
      ((text  (Error "Prior [Regexp] match did not match"))
       (start (Error "Prior [Regexp] match did not match"))
       (end_  (Error "Prior [Regexp] match did not match")))
      |}];
    search_forward_exn "bc" ~update_last_match:true;
    show_last_match ();
    [%expect
      {|
      ((text  (Ok bc))
       (start (Ok 2))
       (end_  (Ok 4)))
      |}]);
  return ()
;;

let%expect_test "[search_{back,for}ward_regexp]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abc";
    goto_char (min ());
    show ();
    [%expect {| (point 1) |}];
    print_s [%sexp (search_forward_regexp ("b" |> Regexp.quote) : bool)];
    [%expect {| true |}];
    show ();
    [%expect {| (point 3) |}];
    print_s [%sexp (search_forward_regexp ("z" |> Regexp.quote) : bool)];
    [%expect {| false |}];
    show ();
    [%expect {| (point 3) |}];
    print_s [%sexp (search_backward_regexp ("a" |> Regexp.quote) : bool)];
    [%expect {| true |}];
    show ();
    [%expect {| (point 1) |}];
    print_s [%sexp (search_backward_regexp ("z" |> Regexp.quote) : bool)];
    [%expect {| false |}];
    show ();
    [%expect {| (point 1) |}]);
  return ()
;;

let%expect_test "[search_{back,for}ward_regexp_exn]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abc";
    goto_char (min ());
    show ();
    [%expect {| (point 1) |}];
    search_forward_regexp_exn ("b" |> Regexp.quote);
    show ();
    [%expect {| (point 3) |}];
    show_raise (fun () -> search_forward_regexp_exn ("z" |> Regexp.quote));
    [%expect {| (raised ("regexp not found" (regexp z))) |}];
    show ();
    [%expect {| (point 3) |}];
    search_backward_regexp_exn ("a" |> Regexp.quote);
    show ();
    [%expect {| (point 1) |}];
    show_raise (fun () -> search_backward_regexp_exn ("z" |> Regexp.quote));
    [%expect {| (raised ("regexp not found" (regexp z))) |}];
    show ();
    [%expect {| (point 1) |}]);
  return ()
;;

let%expect_test "[search_forward_regexp] and [Regexp.Last_match]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abc";
    goto_char (min ());
    print_s
      [%sexp
        (search_forward_regexp ("z" |> Regexp.of_pattern) ~update_last_match:true : bool)];
    [%expect {| false |}];
    show_last_match ();
    [%expect
      {|
      ((text  (Error "Prior [Regexp] match did not match"))
       (start (Error "Prior [Regexp] match did not match"))
       (end_  (Error "Prior [Regexp] match did not match")))
      |}];
    print_s
      [%sexp
        (search_forward_regexp ({|\(b\)|} |> Regexp.of_pattern) ~update_last_match:true
          : bool)];
    [%expect {| true |}];
    show_last_match ();
    [%expect
      {|
      ((text  (Ok b))
       (start (Ok 2))
       (end_  (Ok 3)))
      |}];
    show_last_match () ~subexp:1;
    [%expect
      {|
      ((text  (Ok b))
       (start (Ok 2))
       (end_  (Ok 3)))
      |}]);
  return ()
;;

let%expect_test "[Regexp.Last_match] in wrong buffer" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abc";
    search_backward_exn "b" ~update_last_match:true;
    show_last_match ();
    [%expect
      {|
      ((text  (Ok b))
       (start (Ok 2))
       (end_  (Ok 3)))
      |}]);
  show_last_match ();
  [%expect
    {|
    ((text (
       Error (
         "[Regexp.Last_match.text_exn] called in wrong buffer"
         (current  "#<buffer *scratch*>")
         (expected "#<killed buffer>"))))
     (start (Ok 2))
     (end_  (Ok 3)))
    |}];
  return ()
;;

let%expect_test "[looking_at]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abc";
    goto_char (min ());
    let looking_at s = print_s [%sexp (looking_at (s |> Regexp.quote) : bool)] in
    looking_at "a";
    [%expect {| true |}];
    looking_at "b";
    [%expect {| false |}]);
  return ()
;;

let%expect_test "[looking_at ~update_last_match:true]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert "abc";
    goto_char (min ());
    let looking_at s =
      print_s [%sexp (looking_at (s |> Regexp.quote) ~update_last_match:true : bool)];
      show_last_match ()
    in
    looking_at "a";
    [%expect
      {|
      true
      ((text  (Ok a))
       (start (Ok 1))
       (end_  (Ok 2)))
      |}];
    looking_at "b";
    [%expect
      {|
      false
      ((text  (Error "Prior [Regexp] match did not match"))
       (start (Error "Prior [Regexp] match did not match"))
       (end_  (Error "Prior [Regexp] match did not match")))
      |}]);
  return ()
;;

let%expect_test "[goto_line]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    insert {|
a
bc
def
|};
    let test line =
      goto_line line;
      print_s [%sexp (get () : Position.t)]
    in
    test 0;
    [%expect {| 1 |}];
    test 1;
    [%expect {| 1 |}];
    test 2;
    [%expect {| 2 |}];
    test 3;
    [%expect {| 4 |}];
    test 4;
    [%expect {| 7 |}]);
  return ()
;;

let%expect_test "[Property_search.forward]" =
  let property_name =
    Text.Property_name.Create.("jane-test-text-property" <: Value.Type.int)
  in
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    (* Insert some text with no properties, then 10 digits, each with the property set to
       the value of the digit. *)
    insert "text with no properties\n";
    for i = 0 to 9 do
      let text = i |> Int.to_string |> Text.of_utf8_bytes in
      Text.add_properties text [ T (property_name, i) ];
      insert_text text
    done;
    let test which =
      goto_min ();
      let match_ = Property_search.forward property_name ~which in
      print_s
        [%message
          (which : int Property_search.Which.t)
            (match_ : int Property_search.Match.t option)]
    in
    let all_search_targets =
      let all_of_int = [ 0; 1; 5; 12 ] in
      [%all: int Property_search.Which.t]
    in
    List.iter all_search_targets ~f:(fun which ->
      require_does_not_raise [%here] (fun () -> test which));
    [%expect
      {|
      ((which (First_equal_to 0))
       (match_ ((
         (beginning      25)
         (end_           26)
         (property_value 0)))))
      ((which (First_equal_to 1))
       (match_ ((
         (beginning      26)
         (end_           27)
         (property_value 1)))))
      ((which (First_equal_to 5))
       (match_ ((
         (beginning      30)
         (end_           31)
         (property_value 5)))))
      ((which (First_equal_to 12)) (match_ ()))
      ((which First_non_nil)
       (match_ ((
         (beginning      25)
         (end_           26)
         (property_value 0)))))
      |}]);
  return ()
;;
