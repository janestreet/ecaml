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

let%expect_test "[of_rx]" =
  let test inputs rx =
    let t = of_rx rx in
    print_s [%message (t : t)];
    List.iter inputs ~f:(fun input ->
      let matches = does_match t (Text.of_utf8_bytes input) in
      print_s [%message (input : string) (matches : bool)])
  in
  test [ "a"; "z"; "-"; "b" ] (Any_in [ Chars_in "a-z" ]);
  [%expect {|
    (t "\\(?:[-az]\\)")
    ((input   a)
     (matches true))
    ((input   z)
     (matches true))
    ((input   -)
     (matches true))
    ((input   b)
     (matches false)) |}];
  test [ "a"; "b"; "z"; "A"; "B"; "Z"; "."; "-" ]
    (Any_in [ Chars_in "."; Range ('a', 'z'); Range ('A', 'Z') ]);
  [%expect {|
    (t "\\(?:[.A-Za-z]\\)")
    ((input   a)
     (matches true))
    ((input   b)
     (matches true))
    ((input   z)
     (matches true))
    ((input   A)
     (matches true))
    ((input   B)
     (matches true))
    ((input   Z)
     (matches true))
    ((input   .)
     (matches true))
    ((input   -)
     (matches false)) |}];
  test [ "a"; "b"; "z"; "."; "-" ]
    (Any_in [ Chars_in "-"; Range ('a', 'z') ]);
  [%expect {|
    (t "\\(?:[-a-z]\\)")
    ((input   a)
     (matches true))
    ((input   b)
     (matches true))
    ((input   z)
     (matches true))
    ((input   .)
     (matches false))
    ((input   -)
     (matches true)) |}];
  test [ "foo.*"; "foobar" ] (Exactly "foo.*");
  [%expect {|
    (t "\\(?:foo\\.\\*\\)")
    ((input   foo.*)
     (matches true))
    ((input   foobar)
     (matches false)) |}];
  test [ "\nfoo"; "foo"; "barfoo" ] (Seq [ Line Start; Exactly "foo" ]);
  [%expect {|
    (t "\\(?:^foo\\)")
    ((input   "\nfoo")
     (matches true))
    ((input   foo)
     (matches true))
    ((input   barfoo)
     (matches false)) |}];
  test [ "foo\n"; "foo"; "foobar" ] (Seq [ Exactly "foo"; Line End ]);
  [%expect {|
    (t "\\(?:foo$\\)")
    ((input   "foo\n")
     (matches true))
    ((input   foo)
     (matches true))
    ((input   foobar)
     (matches false)) |}];
  test [ "a"; "b" ] (None_in [ Chars_in "a" ]);
  [%expect {|
    (t "\\(?:[^a]\\)")
    ((input   a)
     (matches false))
    ((input   b)
     (matches true)) |}];
  test [ "a"; "b"; "c" ] (Or [ Exactly "a"; Exactly "b" ]);
  [%expect {|
    (t "\\(?:[ab]\\)")
    ((input   a)
     (matches true))
    ((input   b)
     (matches true))
    ((input   c)
     (matches false)) |}];
  test [ "ab"; "pq"; "ap" ] (Or [ Exactly "ab"; Exactly "pq" ]);
  [%expect {|
    (t "\\(?:\\(?:ab\\|pq\\)\\)")
    ((input   ab)
     (matches true))
    ((input   pq)
     (matches true))
    ((input   ap)
     (matches false)) |}];
  test [ "foo"; "bar" ] (Pattern "foo.*");
  [%expect {|
    (t "\\(?:foo.*\\)")
    ((input   foo)
     (matches true))
    ((input   bar)
     (matches false)) |}];
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foobar";
    Point.goto_min ();
    let matches = Point.search_forward_regexp (of_rx (Seq [ Point; Exactly "foobar" ])) in
    print_s [%message (matches : bool)];
    Point.goto_min ();
    let matches = Point.search_forward_regexp (of_rx (Seq [ Exactly "foobar"; Point ])) in
    print_s [%message (matches : bool)]);
  [%expect {|
    (matches true)
    (matches false) |}];
  List.iter ~f:(fun (min, max) ->
    test [ "cr"; "car"; "cdr"; "cadr"; "caddr" ]
      (Seq [ Exactly "c"
           ; Repeat { min; max; t = Any_in [ Chars_in "ad" ] }
           ; Exactly "r" ]))
    [ 0, None
    ; 0, Some 1
    ; 1, None
    ; 2, None
    ; 1, Some 2
    ];
  [%expect {|
    (t "\\(?:c[ad]*r\\)")
    ((input   cr)
     (matches true))
    ((input   car)
     (matches true))
    ((input   cdr)
     (matches true))
    ((input   cadr)
     (matches true))
    ((input   caddr)
     (matches true))
    (t "\\(?:c[ad]?r\\)")
    ((input   cr)
     (matches true))
    ((input   car)
     (matches true))
    ((input   cdr)
     (matches true))
    ((input   cadr)
     (matches false))
    ((input   caddr)
     (matches false))
    (t "\\(?:c[ad]+r\\)")
    ((input   cr)
     (matches false))
    ((input   car)
     (matches true))
    ((input   cdr)
     (matches true))
    ((input   cadr)
     (matches true))
    ((input   caddr)
     (matches true))
    (t "\\(?:c[ad]\\{2,\\}r\\)")
    ((input   cr)
     (matches false))
    ((input   car)
     (matches false))
    ((input   cdr)
     (matches false))
    ((input   cadr)
     (matches true))
    ((input   caddr)
     (matches true))
    (t "\\(?:c[ad]\\{1,2\\}r\\)")
    ((input   cr)
     (matches false))
    ((input   car)
     (matches true))
    ((input   cdr)
     (matches true))
    ((input   cadr)
     (matches true))
    ((input   caddr)
     (matches false)) |}];
  List.iter ~f:(fun rx ->
    let t =
      of_rx (Seq [ Exactly "foo"
                 ; Repeat { min = 0; max = Some 1; t = rx }
                 ; Exactly "baz" ])
    in
    print_s [%message (t : t)];
    List.iter [ "foobarbaz"; "foobaz"; "foo" ] ~f:(fun input ->
      let matches = does_match ~update_last_match:true t (Text.of_utf8_bytes input) in
      let submatch_1 = Option.try_with (fun () -> Last_match.text_exn ~subexp:1 ()) in
      print_s [%message (input : string) (matches : bool) (submatch_1 : Text.t option)]))
    [ Rx.Submatch (Exactly "bar")
    ; Submatch (Exactly "qux")
    ; Submatch_n { index = 1; t = Exactly "bar"}
    ; Submatch_n { index = 2; t = Exactly "bar"}];
  [%expect {|
    (t "\\(?:foo\\(bar\\)?baz\\)")
    ((input   foobarbaz)
     (matches true)
     (submatch_1 (bar)))
    ((input   foobaz)
     (matches true)
     (submatch_1 ()))
    ((input   foo)
     (matches false)
     (submatch_1 ()))
    (t "\\(?:foo\\(qux\\)?baz\\)")
    ((input   foobarbaz)
     (matches false)
     (submatch_1 ()))
    ((input   foobaz)
     (matches true)
     (submatch_1 ()))
    ((input   foo)
     (matches false)
     (submatch_1 ()))
    (t "\\(?:foo\\(?1:bar\\)?baz\\)")
    ((input   foobarbaz)
     (matches true)
     (submatch_1 (bar)))
    ((input   foobaz)
     (matches true)
     (submatch_1 ()))
    ((input   foo)
     (matches false)
     (submatch_1 ()))
    (t "\\(?:foo\\(?2:bar\\)?baz\\)")
    ((input   foobarbaz)
     (matches true)
     (submatch_1 ()))
    ((input   foobaz)
     (matches true)
     (submatch_1 ()))
    ((input   foo)
     (matches false)
     (submatch_1 ())) |}];
  List.iter ~f:(fun index ->
    let t =
      of_rx
        (Submatch (Seq [ Exactly "foo"
                       ; Submatch (Exactly "bar")
                       ; Submatch_n { index; t = Exactly "baz" }
                       ; Submatch (Exactly "qux")]))
    in
    print_s [%message (t : t)];
    match does_match ~update_last_match:true t (Text.of_utf8_bytes "foobarbazqux") with
    | exception exn -> print_s [%sexp (exn : exn)]
    | matches ->
      let submatch = Option.try_with (Last_match.text_exn ~subexp:index) in
      print_s [%message (matches : bool) (index : int) (submatch : Text.t option)])
    [ 0; 1; 2; 3; 4 ];
  [%expect {|
    (t "\\(?:\\(foo\\(bar\\)\\(?0:baz\\)\\(qux\\)\\)\\)")
    (invalid-regexp ("Invalid regular expression"))
    (t "\\(?:\\(foo\\(bar\\)\\(?1:baz\\)\\(qux\\)\\)\\)")
    (invalid-regexp ("Invalid regular expression"))
    (t "\\(?:\\(foo\\(bar\\)\\(?2:baz\\)\\(qux\\)\\)\\)")
    ((matches true)
     (index   2)
     (submatch (baz)))
    (t "\\(?:\\(foo\\(bar\\)\\(?3:baz\\)\\(qux\\)\\)\\)")
    ((matches true)
     (index   3)
     (submatch (baz)))
    (t "\\(?:\\(foo\\(bar\\)\\(?4:baz\\)\\(qux\\)\\)\\)")
    ((matches true)
     (index   4)
     (submatch (baz))) |}];
  [%expect {| |}]
;;
