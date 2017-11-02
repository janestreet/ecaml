open! Core_kernel
open! Import
open! Regexp

let%expect_test "[quote], [match_], [does_match]" =
  List.iter
    [ ""  , ""
    ; ""  , "a"
    ; "a" , ""
    ; "a" , "b"
    ; "a" , "ba"
    ; {|\|}, {|\|} ]
    ~f:(fun (pattern, input) ->
      let pattern = quote pattern in
      let input = Text.of_utf8_bytes input in
      print_s [%message
        ""
          (pattern : t)
          (input : Text.t)
          ~match_:(match_ pattern input : int option)
          ~does_match:(does_match pattern input : bool)]);
  [%expect {|
    ((pattern "")
     (input   "")
     (match_ (0))
     (does_match true))
    ((pattern "")
     (input   a)
     (match_ (0))
     (does_match true))
    ((pattern a)
     (input   "")
     (match_ ())
     (does_match false))
    ((pattern a)
     (input   b)
     (match_ ())
     (does_match false))
    ((pattern a)
     (input   ba)
     (match_ (1))
     (does_match true))
    ((pattern "\\\\")
     (input   "\\")
     (match_ (0))
     (does_match true)) |}];
;;

let%expect_test "[any_pattern]" =
  List.iter
    [ []
    ; [ "a" ]
    ; [ "b" ]
    ; [ "a"; "b" ]
    ; [ "b"; "a" ] ]
    ~f:(fun patterns ->
      let regexp = any_pattern patterns in
      print_s [%message
        ""
          (patterns : string list)
          (regexp : Regexp.t)
          ~does_match:(does_match regexp ("a" |> Text.of_utf8_bytes) : bool)]);
  [%expect {|
    ((patterns ())
     (regexp     "")
     (does_match true))
    ((patterns (a))
     (regexp     a)
     (does_match true))
    ((patterns (b))
     (regexp     b)
     (does_match false))
    ((patterns (a b))
     (regexp     "a\\|b")
     (does_match true))
    ((patterns (b a))
     (regexp     "b\\|a")
     (does_match true)) |}];
;;

let%expect_test "[any_quote]" =
  List.iter
    [ []
    ; [ "a" ]
    ; [ "b" ]
    ; [ "a"; "b" ]]
    ~f:(fun strings ->
      let regexp = any_quote strings in
      print_s [%message
        ""
          (strings : string list)
          (regexp : Regexp.t)
          ~does_match:(does_match regexp ("a" |> Text.of_utf8_bytes) : bool)]);
  [%expect {|
    ((strings ())
     (regexp     z^)
     (does_match false))
    ((strings (a))
     (regexp     a)
     (does_match true))
    ((strings (b))
     (regexp     b)
     (does_match false))
    ((strings (a b))
     (regexp     [ab])
     (does_match true)) |}];
;;

let%expect_test "[Last_match] raise due to no match" =
  require [%here] (is_none (match_ ~update_last_match:true
                              ("foo" |> quote)
                              ("xxx" |> Text.of_utf8_bytes)));
  show_last_match ();
  [%expect {|
    ((text  (Error "Prior [Regexp] match did not match"))
     (start (Error "Prior [Regexp] match did not match"))
     (end_  (Error "Prior [Regexp] match did not match"))) |}];
;;

let%expect_test "[Last_match] with nonexistent [subexp]" =
  require [%here] (is_some (match_ ~update_last_match:true
                              ("foo" |> quote)
                              ("xfoox" |> Text.of_utf8_bytes)));
  show_last_match () ~subexp:1;
  [%expect {|
    ((text (
       Error (
         "[Regexp.Last_match.text_exn] got [subexp] that did not match"
         (subexp 1))))
     (start (
       Error (
         "[Regexp.Last_match.start_exn] got [subexp] that did not match"
         (subexp 1))))
     (end_ (
       Error (
         "[Regexp.Last_match.end_exn] got [subexp] that did not match" (subexp 1))))) |}];
;;

let%expect_test "[Last_match] success" =
  List.iter
    [ ""    , ""
    ; ""    , "a"
    ; "a"   , "a"
    ; "b"   , "ab"
    ; "foo" , "xfoox" ]
    ~f:(fun (pattern, input) ->
      let match_result =
        match_ ~update_last_match:true
          (pattern |> quote)
          (input |> Text.of_utf8_bytes) in
      print_s [%message
        ""
          (pattern : string)
          (input : string)
          (match_result : int option)];
      show_last_match ());
  [%expect {|
    ((pattern "")
     (input   "")
     (match_result (0)))
    ((text  (Ok ""))
     (start (Ok 0))
     (end_  (Ok 0)))
    ((pattern "")
     (input   a)
     (match_result (0)))
    ((text  (Ok ""))
     (start (Ok 0))
     (end_  (Ok 0)))
    ((pattern a)
     (input   a)
     (match_result (0)))
    ((text  (Ok a))
     (start (Ok 0))
     (end_  (Ok 1)))
    ((pattern b)
     (input   ab)
     (match_result (1)))
    ((text  (Ok b))
     (start (Ok 1))
     (end_  (Ok 2)))
    ((pattern foo)
     (input   xfoox)
     (match_result (1)))
    ((text  (Ok foo))
     (start (Ok 1))
     (end_  (Ok 4))) |}];
;;

let%expect_test "[Last_match ~subexp] success" =
  require [%here] (
    is_some (match_ ~update_last_match:true
               ({|\(foo\)|} |> of_pattern)
               ("xfoox" |> Text.of_utf8_bytes)));
  show_last_match () ~subexp:1;
  [%expect {|
    ((text  (Ok foo))
     (start (Ok 1))
     (end_  (Ok 4))) |}];
;;

let%expect_test "[Last_match.get_exn]" =
  require [%here] (
    is_some (match_ ~update_last_match:true
               ({|\(foo\)|} |> of_pattern)
               ("xfoox" |> Text.of_utf8_bytes)));
  let match_data = Last_match.get_exn () in
  print_s [%sexp (match_data : Last_match.t)];
  [%expect {|
    ((location (Text xfoox)) (positions (1 4 1 4))) |}];
  require [%here] (is_some (match_ ~update_last_match:true
                              ("a"|> quote)
                              ("a" |> Text.of_utf8_bytes)));
  print_s [%sexp (Last_match.get_exn () : Last_match.t)];
  [%expect {|
    ((location  (Text a))
     (positions (0    1))) |}];
  Last_match.set match_data;
  print_s [%sexp (Last_match.get_exn () : Last_match.t)];
  [%expect {|
    ((location (Text xfoox)) (positions (1 4 1 4))) |}];
  show_last_match ();
  [%expect {|
    ((text  (Ok foo))
     (start (Ok 1))
     (end_  (Ok 4))) |}];
;;
