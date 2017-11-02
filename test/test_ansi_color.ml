open! Core_kernel
open! Import
open! Ansi_color

let color_string s = color_text (s |> Text.of_utf8_bytes)

let escape codes =
  concat [ "\027["
         ; concat (codes |> List.map ~f:Int.to_string |> List.intersperse ~sep:";")
         ; "m" ]
;;

let print_state_machine string =
  Ref.set_temporarily Ansi_color.print_state_machine true ~f:(fun () ->
    ignore (color_text (string |> Text.of_utf8_bytes) : Text.t));
;;

let%expect_test "empty state machine" =
  print_state_machine "";
  [%expect {|
    (state_machine (
      (current_state (
        attributes (
          (background ())
          (blink_rapid false)
          (blink_slow  false)
          (bold        false)
          (faint       false)
          (foreground ())
          (italic        false)
          (reverse_video false)
          (underline     false))))
      (empty_state (
        attributes (
          (background ())
          (blink_rapid false)
          (blink_slow  false)
          (bold        false)
          (faint       false)
          (foreground ())
          (italic        false)
          (reverse_video false)
          (underline     false)))))) |}];
;;

let%expect_test "simple state machine" =
  print_state_machine (escape [ 1; 31; 0 ]);
  [%expect {|
    (state_machine (
      (current_state (
        (attributes (
          (background ())
          (blink_rapid false)
          (blink_slow  false)
          (bold        false)
          (faint       false)
          (foreground ())
          (italic        false)
          (reverse_video false)
          (underline     false)))
        (next_state_by_code ((
          1 (
            (background ())
            (blink_rapid false)
            (blink_slow  false)
            (bold        true)
            (faint       false)
            (foreground ())
            (italic        false)
            (reverse_video false)
            (underline     false)))))))
      (empty_state (
        (attributes (
          (background ())
          (blink_rapid false)
          (blink_slow  false)
          (bold        false)
          (faint       false)
          (foreground ())
          (italic        false)
          (reverse_video false)
          (underline     false)))
        (next_state_by_code ((
          1 (
            (background ())
            (blink_rapid false)
            (blink_slow  false)
            (bold        true)
            (faint       false)
            (foreground ())
            (italic        false)
            (reverse_video false)
            (underline     false)))))))
      (all_states (
        ((attributes (
           (background ())
           (blink_rapid false)
           (blink_slow  false)
           (bold        true)
           (faint       false)
           (foreground ((
             (brightness  Regular)
             (color_index 1))))
           (italic        false)
           (reverse_video false)
           (underline     false)))
         (text_properties (
           (face ((Attributes ((Foreground (Color red3)) (Weight Bold)))))
           (font-lock-face ((Attributes ((Foreground (Color red3)) (Weight Bold)))))))
         (next_state_by_code ((
           0 (
             (background ())
             (blink_rapid false)
             (blink_slow  false)
             (bold        false)
             (faint       false)
             (foreground ())
             (italic        false)
             (reverse_video false)
             (underline     false))))))
        ((attributes (
           (background ())
           (blink_rapid false)
           (blink_slow  false)
           (bold        true)
           (faint       false)
           (foreground ())
           (italic        false)
           (reverse_video false)
           (underline     false)))
         (text_properties (
           (face ((Attributes ((Weight Bold)))))
           (font-lock-face ((Attributes ((Weight Bold)))))))
         (next_state_by_code ((
           31 (
             (background ())
             (blink_rapid false)
             (blink_slow  false)
             (bold        true)
             (faint       false)
             (foreground ((
               (brightness  Regular)
               (color_index 1))))
             (italic        false)
             (reverse_video false)
             (underline     false)))))))))) |}];
;;

let test ?(show_input = true) input =
  let output = try_with (fun () -> color_string input) in
  print_s [%message.omit_nil
    ""
      ~_:(if show_input then [%message input] else [%message] : Sexp.t)
      ~_:(output : Text.t Or_error.t)]
;;

let test_codes codes = test (concat [ escape codes; "foo" ])

let%expect_test "empty input" =
  test "";
  [%expect {|
    ("" (Ok "")) |}]
;;

let%expect_test "no codes" =
  test "foo";
  [%expect {|
    (foo (Ok foo)) |}]
;;

let%expect_test "empty escape sequence" =
  test_codes [];
  [%expect {|
    ("\027[mfoo" (Ok foo)) |}]
;;

let%expect_test "single code" =
  for code = 0 to max_supported_code do
    test_codes [ code ];
  done;
  [%expect {|
    ("\027[0mfoo" (Ok foo))
    ("\027[1mfoo" (
      Ok (foo 0 3 (face (:weight bold) font-lock-face (:weight bold)))))
    ("\027[2mfoo" (
      Ok (foo 0 3 (face (:foreground grey50) font-lock-face (:foreground grey50)))))
    ("\027[3mfoo" (
      Ok (foo 0 3 (face (:slant italic) font-lock-face (:slant italic)))))
    ("\027[4mfoo" (
      Ok (foo 0 3 (face (:underline t) font-lock-face (:underline t)))))
    ("\027[5mfoo" (Ok foo))
    ("\027[6mfoo" (Ok foo))
    ("\027[7mfoo" (Ok foo))
    ("\027[8mfoo" (Ok foo))
    ("\027[9mfoo" (Ok foo))
    ("\027[10mfoo" (Ok foo))
    ("\027[11mfoo" (Ok foo))
    ("\027[12mfoo" (Ok foo))
    ("\027[13mfoo" (Ok foo))
    ("\027[14mfoo" (Ok foo))
    ("\027[15mfoo" (Ok foo))
    ("\027[16mfoo" (Ok foo))
    ("\027[17mfoo" (Ok foo))
    ("\027[18mfoo" (Ok foo))
    ("\027[19mfoo" (Ok foo))
    ("\027[20mfoo" (Ok foo))
    ("\027[21mfoo" (Ok foo))
    ("\027[22mfoo" (Ok foo))
    ("\027[23mfoo" (Ok foo))
    ("\027[24mfoo" (Ok foo))
    ("\027[25mfoo" (Ok foo))
    ("\027[26mfoo" (Ok foo))
    ("\027[27mfoo" (Ok foo))
    ("\027[28mfoo" (Ok foo))
    ("\027[29mfoo" (Ok foo))
    ("\027[30mfoo" (
      Ok (foo 0 3 (face (:foreground black) font-lock-face (:foreground black)))))
    ("\027[31mfoo" (
      Ok (foo 0 3 (face (:foreground red3) font-lock-face (:foreground red3)))))
    ("\027[32mfoo" (
      Ok (foo 0 3 (face (:foreground green3) font-lock-face (:foreground green3)))))
    ("\027[33mfoo" (
      Ok (
        foo 0 3 (face (:foreground yellow3) font-lock-face (:foreground yellow3)))))
    ("\027[34mfoo" (
      Ok (foo 0 3 (face (:foreground blue2) font-lock-face (:foreground blue2)))))
    ("\027[35mfoo" (
      Ok (
        foo 0 3 (
          face (:foreground magenta3) font-lock-face (:foreground magenta3)))))
    ("\027[36mfoo" (
      Ok (foo 0 3 (face (:foreground cyan3) font-lock-face (:foreground cyan3)))))
    ("\027[37mfoo" (
      Ok (foo 0 3 (face (:foreground gray90) font-lock-face (:foreground gray90)))))
    ("\027[38mfoo" (Ok foo))
    ("\027[39mfoo" (Ok foo))
    ("\027[40mfoo" (
      Ok (foo 0 3 (face (:background black) font-lock-face (:background black)))))
    ("\027[41mfoo" (
      Ok (foo 0 3 (face (:background red3) font-lock-face (:background red3)))))
    ("\027[42mfoo" (
      Ok (foo 0 3 (face (:background green3) font-lock-face (:background green3)))))
    ("\027[43mfoo" (
      Ok (
        foo 0 3 (face (:background yellow3) font-lock-face (:background yellow3)))))
    ("\027[44mfoo" (
      Ok (foo 0 3 (face (:background blue2) font-lock-face (:background blue2)))))
    ("\027[45mfoo" (
      Ok (
        foo 0 3 (
          face (:background magenta3) font-lock-face (:background magenta3)))))
    ("\027[46mfoo" (
      Ok (foo 0 3 (face (:background cyan3) font-lock-face (:background cyan3)))))
    ("\027[47mfoo" (
      Ok (foo 0 3 (face (:background gray90) font-lock-face (:background gray90)))))
    ("\027[48mfoo" (Ok foo))
    ("\027[49mfoo" (Ok foo))
    ("\027[50mfoo" (Ok foo))
    ("\027[51mfoo" (Ok foo))
    ("\027[52mfoo" (Ok foo))
    ("\027[53mfoo" (Ok foo))
    ("\027[54mfoo" (Ok foo))
    ("\027[55mfoo" (Ok foo))
    ("\027[56mfoo" (Ok foo))
    ("\027[57mfoo" (Ok foo))
    ("\027[58mfoo" (Ok foo))
    ("\027[59mfoo" (Ok foo))
    ("\027[60mfoo" (Ok foo))
    ("\027[61mfoo" (Ok foo))
    ("\027[62mfoo" (Ok foo))
    ("\027[63mfoo" (Ok foo))
    ("\027[64mfoo" (Ok foo))
    ("\027[65mfoo" (Ok foo))
    ("\027[66mfoo" (Ok foo))
    ("\027[67mfoo" (Ok foo))
    ("\027[68mfoo" (Ok foo))
    ("\027[69mfoo" (Ok foo))
    ("\027[70mfoo" (Ok foo))
    ("\027[71mfoo" (Ok foo))
    ("\027[72mfoo" (Ok foo))
    ("\027[73mfoo" (Ok foo))
    ("\027[74mfoo" (Ok foo))
    ("\027[75mfoo" (Ok foo))
    ("\027[76mfoo" (Ok foo))
    ("\027[77mfoo" (Ok foo))
    ("\027[78mfoo" (Ok foo))
    ("\027[79mfoo" (Ok foo))
    ("\027[80mfoo" (Ok foo))
    ("\027[81mfoo" (Ok foo))
    ("\027[82mfoo" (Ok foo))
    ("\027[83mfoo" (Ok foo))
    ("\027[84mfoo" (Ok foo))
    ("\027[85mfoo" (Ok foo))
    ("\027[86mfoo" (Ok foo))
    ("\027[87mfoo" (Ok foo))
    ("\027[88mfoo" (Ok foo))
    ("\027[89mfoo" (Ok foo))
    ("\027[90mfoo" (
      Ok (foo 0 3 (face (:foreground grey50) font-lock-face (:foreground grey50)))))
    ("\027[91mfoo" (
      Ok (foo 0 3 (face (:foreground red1) font-lock-face (:foreground red1)))))
    ("\027[92mfoo" (
      Ok (foo 0 3 (face (:foreground green1) font-lock-face (:foreground green1)))))
    ("\027[93mfoo" (
      Ok (
        foo 0 3 (face (:foreground yellow1) font-lock-face (:foreground yellow1)))))
    ("\027[94mfoo" (
      Ok (
        foo 0 3 (
          face
          (:foreground "deep sky blue")
          font-lock-face
          (:foreground "deep sky blue")))))
    ("\027[95mfoo" (
      Ok (
        foo 0 3 (
          face (:foreground magenta1) font-lock-face (:foreground magenta1)))))
    ("\027[96mfoo" (
      Ok (foo 0 3 (face (:foreground cyan1) font-lock-face (:foreground cyan1)))))
    ("\027[97mfoo" (
      Ok (foo 0 3 (face (:foreground white) font-lock-face (:foreground white)))))
    ("\027[98mfoo" (Ok foo))
    ("\027[99mfoo" (Ok foo))
    ("\027[100mfoo" (
      Ok (foo 0 3 (face (:background grey50) font-lock-face (:background grey50)))))
    ("\027[101mfoo" (
      Ok (foo 0 3 (face (:background red1) font-lock-face (:background red1)))))
    ("\027[102mfoo" (
      Ok (foo 0 3 (face (:background green1) font-lock-face (:background green1)))))
    ("\027[103mfoo" (
      Ok (
        foo 0 3 (face (:background yellow1) font-lock-face (:background yellow1)))))
    ("\027[104mfoo" (
      Ok (
        foo 0 3 (
          face
          (:background "deep sky blue")
          font-lock-face
          (:background "deep sky blue")))))
    ("\027[105mfoo" (
      Ok (
        foo 0 3 (
          face (:background magenta1) font-lock-face (:background magenta1)))))
    ("\027[106mfoo" (
      Ok (foo 0 3 (face (:background cyan1) font-lock-face (:background cyan1)))))
    ("\027[107mfoo" (
      Ok (foo 0 3 (face (:background white) font-lock-face (:background white)))))
    ("\027[108mfoo" (Ok foo))
    ("\027[109mfoo" (Ok foo)) |}]
;;

let%expect_test "large code" =
  test_codes [ 123456 ];
  [%expect {|
    ("\027[123456mfoo" (Ok foo)) |}]
;;

let%expect_test "rendering invalid escape sequences" =
  List.iter
    [ "\027["
    ; "\027[foo"
    ; "\027[31foo" ]
    ~f:(fun input ->
      print_s [%message "" input ~_:(color_string input : Text.t)]);
  [%expect {|
    ("\027[" "<invalid ANSI escape sequence \"\\027\">[")
    ("\027[foo" "<invalid ANSI escape sequence \"\\027[\">foo")
    ("\027[31foo" "<invalid ANSI escape sequence \"\\027[31\">foo") |}];
;;

let%expect_test "customization to disable rendering invalid escape sequences" =
  Current_buffer.set_value_temporarily Ansi_color.show_invalid_escapes false
    ~f:(fun () ->
      List.iter
        [ "\027["
        ; "\027[foo"
        ; "\027[31foo" ]
        ~f:(fun input ->
          print_s [%message "" input ~_:(color_string input : Text.t)]));
  [%expect {|
    ("\027[" "\027[")
    ("\027[foo" "\027[foo")
    ("\027[31foo" "\027[31foo") |}];
;;

let%expect_test "color region twice" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Point.insert (concat [ escape [ 31 ]; "foo" ]);
    let show () =
      print_s [%sexp (
        Current_buffer.contents ~text_properties:true () : Text.t)] in
    color_current_buffer () ~start:(Point.min ()) ~end_:(Point.max ());
    show ();
    [%expect {|
      (foo 0 3 (face (:foreground red3) font-lock-face (:foreground red3))) |}];
    color_current_buffer () ~start:(Point.min ()) ~end_:(Point.max ());
    show ();
    [%expect {|
      (foo 0 3 (face (:foreground red3) font-lock-face (:foreground red3))) |}]);
;;

let%expect_test "color buffer twice" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Point.insert (concat [ escape [ 31 ]; "foo" ]);
    let show () =
      print_s [%sexp (
        Current_buffer.contents ~text_properties:true () : Text.t)] in
    color_current_buffer ();
    show ();
    [%expect {|
      (foo 0 3 (face (:foreground red3) font-lock-face (:foreground red3))) |}];
    color_current_buffer ();
    show ();
    [%expect {|
      (foo 0 3 (face (:foreground red3) font-lock-face (:foreground red3))) |}]);
;;

let%expect_test "[color_current_buffer] and point" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Point.insert (concat [ escape [ 31 ]; "foo" ]);
    let show_point () = print_s [%sexp (Point.get () : Position.t)] in
    show_point ();
    [%expect {|
      9 |}];
    color_current_buffer () ~start:(Point.min ()) ~end_:(Point.max ());
    [%expect {| |}];
    show_point ();
    [%expect {|
      1 |}]);
;;

let%expect_test "patdiff output" =
  test ~show_input:false {|
[1;34m@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@[0m
[1;34m@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ .hgignore @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@[0m
scrutiny scrutiny
base 7dcf98789cff | tip 164054e60e6e
┌
│[1;34m@@@@@@@@[0m [35mHunk 1/2913[0m [1;34m@@@@@@@@[0m
│[1;34m@@@@@@@@[0m [31mbase[0m [34m202,227[0m [32mtip[0m [34m202,226[0m [1;34m@@@@@@@@[0m
│  glob:app/emacs/elisp/contrib/auctex/preview/latex/prtightpage.def
│  glob:app/emacs/elisp/contrib/auctex/preview/latex/prtracingall.def
│  glob:app/emacs/elisp/contrib/auctex/preview/preview-latex.el
│  glob:app/emacs/elisp/contrib/auctex/tex-site.el
│  glob:app/emacs/elisp/contrib/auctex/tex-site.el.out
│  glob:app/emacs/elisp/contrib/helm/helm-autoloads.el
│  glob:app/emacs/elisp/contrib/image+.el
│  glob:app/emacs/elisp/contrib/org-tests.foo.png
│  glob:app/emacs/elisp/contrib/org-tests.html
│  glob:app/emacs/elisp/contrib/org/doc/org
│  glob:app/emacs/elisp/contrib/org/doc/org-version.inc
│  glob:app/emacs/elisp/contrib/org/doc/org.html
│[0;1;31m-|[0m[0;31mglob:app/emacs/elisp/contrib/org/doc/org.pdf[0m
│  glob:app/emacs/elisp/contrib/org/doc/org.t2d
│  glob:app/emacs/elisp/contrib/org/doc/orgcard.pdf
│  glob:app/emacs/elisp/contrib/org/doc/orgcard_letter.pdf
│  glob:app/emacs/elisp/contrib/org/doc/orgguide.pdf
│  glob:app/emacs/elisp/contrib/org/lisp/org-loaddefs.el
│  glob:app/emacs/elisp/contrib/org/lisp/org-version.el
│  glob:app/emacs/elisp/contrib/org/local.mk
│  glob:app/emacs/test/ocaml/2/big/a???.ml
│  glob:app/emacs/test/ocaml/2/big/jbuild
└ |};
  [%expect {|
    (Ok (
      "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ .hgignore @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\nscrutiny scrutiny\nbase 7dcf98789cff | tip 164054e60e6e\n\226\148\140\n\226\148\130@@@@@@@@ Hunk 1/2913 @@@@@@@@\n\226\148\130@@@@@@@@ base 202,227 tip 202,226 @@@@@@@@\n\226\148\130  glob:app/emacs/elisp/contrib/auctex/preview/latex/prtightpage.def\n\226\148\130  glob:app/emacs/elisp/contrib/auctex/preview/latex/prtracingall.def\n\226\148\130  glob:app/emacs/elisp/contrib/auctex/preview/preview-latex.el\n\226\148\130  glob:app/emacs/elisp/contrib/auctex/tex-site.el\n\226\148\130  glob:app/emacs/elisp/contrib/auctex/tex-site.el.out\n\226\148\130  glob:app/emacs/elisp/contrib/helm/helm-autoloads.el\n\226\148\130  glob:app/emacs/elisp/contrib/image+.el\n\226\148\130  glob:app/emacs/elisp/contrib/org-tests.foo.png\n\226\148\130  glob:app/emacs/elisp/contrib/org-tests.html\n\226\148\130  glob:app/emacs/elisp/contrib/org/doc/org\n\226\148\130  glob:app/emacs/elisp/contrib/org/doc/org-version.inc\n\226\148\130  glob:app/emacs/elisp/contrib/org/doc/org.html\n\226\148\130-|glob:app/emacs/elisp/contrib/org/doc/org.pdf\n\226\148\130  glob:app/emacs/elisp/contrib/org/doc/org.t2d\n\226\148\130  glob:app/emacs/elisp/contrib/org/doc/orgcard.pdf\n\226\148\130  glob:app/emacs/elisp/contrib/org/doc/orgcard_letter.pdf\n\226\148\130  glob:app/emacs/elisp/contrib/org/doc/orgguide.pdf\n\226\148\130  glob:app/emacs/elisp/contrib/org/lisp/org-loaddefs.el\n\226\148\130  glob:app/emacs/elisp/contrib/org/lisp/org-version.el\n\226\148\130  glob:app/emacs/elisp/contrib/org/local.mk\n\226\148\130  glob:app/emacs/test/ocaml/2/big/a???.ml\n\226\148\130  glob:app/emacs/test/ocaml/2/big/jbuild\n\226\148\148 "
      1
      85
      (face
        (:foreground blue2 :weight bold)
        font-lock-face
        (:foreground blue2 :weight bold))
      86
      170
      (face
        (:foreground blue2 :weight bold)
        font-lock-face
        (:foreground blue2 :weight bold))
      229
      237
      (face
        (:foreground blue2 :weight bold)
        font-lock-face
        (:foreground blue2 :weight bold))
      238
      249
      (face (:foreground magenta3) font-lock-face (:foreground magenta3))
      250
      258
      (face
        (:foreground blue2 :weight bold)
        font-lock-face
        (:foreground blue2 :weight bold))
      260
      268
      (face
        (:foreground blue2 :weight bold)
        font-lock-face
        (:foreground blue2 :weight bold))
      269
      273
      (face (:foreground red3) font-lock-face (:foreground red3))
      274
      281
      (face (:foreground blue2) font-lock-face (:foreground blue2))
      282
      285
      (face (:foreground green3) font-lock-face (:foreground green3))
      286
      293
      (face (:foreground blue2) font-lock-face (:foreground blue2))
      294
      302
      (face
        (:foreground blue2 :weight bold)
        font-lock-face
        (:foreground blue2 :weight bold))
      956
      958
      (face
        (:foreground red3 :weight bold)
        font-lock-face
        (:foreground red3 :weight bold))
      958
      1002
      (face (:foreground red3) font-lock-face (:foreground red3)))) |}]
;;

let%expect_test "[Ansi_color.Colors.get]" =
  print_s [%sexp (Colors.get () : Colors.t)];
  [%expect {|
    ((regular (black red3 green3 yellow3 blue2 magenta3 cyan3 gray90))
     (bright (grey50 red1 green1 yellow1 "deep sky blue" magenta1 cyan1 white))
     (faint (
       black "dark red" "dark green" yellow4 "dark blue" magenta4 cyan4 grey50))
     (faint_default_color grey50)) |}];
;;

let%expect_test "colors" =
  List.iter (List.range 0 8)
    ~f:(fun i ->
      let color = 30 + i in
      let bright_color = color + 60 in
      let background_color = color + 10 in
      let bright_background_color = background_color + 60 in
      List.iter [color; bright_color; background_color; bright_background_color]
        ~f:(fun color ->
          test ~show_input:false (sprintf "%scolor %d" (escape [color]) color);
          test ~show_input:false (sprintf "%scolor %d with faint" (escape [color; 2]) color)
        );
    );
  [%expect {|
    (Ok (
      "color 30" 0 8 (face (:foreground black) font-lock-face (:foreground black))))
    (Ok (
      "color 30 with faint" 0 19 (
        face (:foreground black) font-lock-face (:foreground black))))
    (Ok (
      "color 90" 0 8 (
        face (:foreground grey50) font-lock-face (:foreground grey50))))
    (Ok (
      "color 90 with faint" 0 19 (
        face (:foreground black) font-lock-face (:foreground black))))
    (Ok (
      "color 40" 0 8 (face (:background black) font-lock-face (:background black))))
    (Ok (
      "color 40 with faint" 0 19 (
        face
        (:background black :foreground grey50)
        font-lock-face
        (:background black :foreground grey50))))
    (Ok (
      "color 100" 0 9 (
        face (:background grey50) font-lock-face (:background grey50))))
    (Ok (
      "color 100 with faint" 0 20 (
        face
        (:background grey50 :foreground grey50)
        font-lock-face
        (:background grey50 :foreground grey50))))
    (Ok (
      "color 31" 0 8 (face (:foreground red3) font-lock-face (:foreground red3))))
    (Ok (
      "color 31 with faint" 0 19 (
        face (:foreground "dark red") font-lock-face (:foreground "dark red"))))
    (Ok (
      "color 91" 0 8 (face (:foreground red1) font-lock-face (:foreground red1))))
    (Ok (
      "color 91 with faint" 0 19 (
        face (:foreground red3) font-lock-face (:foreground red3))))
    (Ok (
      "color 41" 0 8 (face (:background red3) font-lock-face (:background red3))))
    (Ok (
      "color 41 with faint" 0 19 (
        face
        (:background red3 :foreground grey50)
        font-lock-face
        (:background red3 :foreground grey50))))
    (Ok (
      "color 101" 0 9 (face (:background red1) font-lock-face (:background red1))))
    (Ok (
      "color 101 with faint" 0 20 (
        face
        (:background red1 :foreground grey50)
        font-lock-face
        (:background red1 :foreground grey50))))
    (Ok (
      "color 32" 0 8 (
        face (:foreground green3) font-lock-face (:foreground green3))))
    (Ok (
      "color 32 with faint" 0 19 (
        face (:foreground "dark green") font-lock-face (:foreground "dark green"))))
    (Ok (
      "color 92" 0 8 (
        face (:foreground green1) font-lock-face (:foreground green1))))
    (Ok (
      "color 92 with faint" 0 19 (
        face (:foreground green3) font-lock-face (:foreground green3))))
    (Ok (
      "color 42" 0 8 (
        face (:background green3) font-lock-face (:background green3))))
    (Ok (
      "color 42 with faint" 0 19 (
        face
        (:background green3 :foreground grey50)
        font-lock-face
        (:background green3 :foreground grey50))))
    (Ok (
      "color 102" 0 9 (
        face (:background green1) font-lock-face (:background green1))))
    (Ok (
      "color 102 with faint" 0 20 (
        face
        (:background green1 :foreground grey50)
        font-lock-face
        (:background green1 :foreground grey50))))
    (Ok (
      "color 33" 0 8 (
        face (:foreground yellow3) font-lock-face (:foreground yellow3))))
    (Ok (
      "color 33 with faint" 0 19 (
        face (:foreground yellow4) font-lock-face (:foreground yellow4))))
    (Ok (
      "color 93" 0 8 (
        face (:foreground yellow1) font-lock-face (:foreground yellow1))))
    (Ok (
      "color 93 with faint" 0 19 (
        face (:foreground yellow3) font-lock-face (:foreground yellow3))))
    (Ok (
      "color 43" 0 8 (
        face (:background yellow3) font-lock-face (:background yellow3))))
    (Ok (
      "color 43 with faint" 0 19 (
        face
        (:background yellow3 :foreground grey50)
        font-lock-face
        (:background yellow3 :foreground grey50))))
    (Ok (
      "color 103" 0 9 (
        face (:background yellow1) font-lock-face (:background yellow1))))
    (Ok (
      "color 103 with faint" 0 20 (
        face
        (:background yellow1 :foreground grey50)
        font-lock-face
        (:background yellow1 :foreground grey50))))
    (Ok (
      "color 34" 0 8 (face (:foreground blue2) font-lock-face (:foreground blue2))))
    (Ok (
      "color 34 with faint" 0 19 (
        face (:foreground "dark blue") font-lock-face (:foreground "dark blue"))))
    (Ok (
      "color 94" 0 8 (
        face
        (:foreground "deep sky blue")
        font-lock-face
        (:foreground "deep sky blue"))))
    (Ok (
      "color 94 with faint" 0 19 (
        face (:foreground blue2) font-lock-face (:foreground blue2))))
    (Ok (
      "color 44" 0 8 (face (:background blue2) font-lock-face (:background blue2))))
    (Ok (
      "color 44 with faint" 0 19 (
        face
        (:background blue2 :foreground grey50)
        font-lock-face
        (:background blue2 :foreground grey50))))
    (Ok (
      "color 104" 0 9 (
        face
        (:background "deep sky blue")
        font-lock-face
        (:background "deep sky blue"))))
    (Ok (
      "color 104 with faint" 0 20 (
        face
        (:background "deep sky blue" :foreground grey50)
        font-lock-face
        (:background "deep sky blue" :foreground grey50))))
    (Ok (
      "color 35" 0 8 (
        face (:foreground magenta3) font-lock-face (:foreground magenta3))))
    (Ok (
      "color 35 with faint" 0 19 (
        face (:foreground magenta4) font-lock-face (:foreground magenta4))))
    (Ok (
      "color 95" 0 8 (
        face (:foreground magenta1) font-lock-face (:foreground magenta1))))
    (Ok (
      "color 95 with faint" 0 19 (
        face (:foreground magenta3) font-lock-face (:foreground magenta3))))
    (Ok (
      "color 45" 0 8 (
        face (:background magenta3) font-lock-face (:background magenta3))))
    (Ok (
      "color 45 with faint" 0 19 (
        face
        (:background magenta3 :foreground grey50)
        font-lock-face
        (:background magenta3 :foreground grey50))))
    (Ok (
      "color 105" 0 9 (
        face (:background magenta1) font-lock-face (:background magenta1))))
    (Ok (
      "color 105 with faint" 0 20 (
        face
        (:background magenta1 :foreground grey50)
        font-lock-face
        (:background magenta1 :foreground grey50))))
    (Ok (
      "color 36" 0 8 (face (:foreground cyan3) font-lock-face (:foreground cyan3))))
    (Ok (
      "color 36 with faint" 0 19 (
        face (:foreground cyan4) font-lock-face (:foreground cyan4))))
    (Ok (
      "color 96" 0 8 (face (:foreground cyan1) font-lock-face (:foreground cyan1))))
    (Ok (
      "color 96 with faint" 0 19 (
        face (:foreground cyan3) font-lock-face (:foreground cyan3))))
    (Ok (
      "color 46" 0 8 (face (:background cyan3) font-lock-face (:background cyan3))))
    (Ok (
      "color 46 with faint" 0 19 (
        face
        (:background cyan3 :foreground grey50)
        font-lock-face
        (:background cyan3 :foreground grey50))))
    (Ok (
      "color 106" 0 9 (
        face (:background cyan1) font-lock-face (:background cyan1))))
    (Ok (
      "color 106 with faint" 0 20 (
        face
        (:background cyan1 :foreground grey50)
        font-lock-face
        (:background cyan1 :foreground grey50))))
    (Ok (
      "color 37" 0 8 (
        face (:foreground gray90) font-lock-face (:foreground gray90))))
    (Ok (
      "color 37 with faint" 0 19 (
        face (:foreground grey50) font-lock-face (:foreground grey50))))
    (Ok (
      "color 97" 0 8 (face (:foreground white) font-lock-face (:foreground white))))
    (Ok (
      "color 97 with faint" 0 19 (
        face (:foreground gray90) font-lock-face (:foreground gray90))))
    (Ok (
      "color 47" 0 8 (
        face (:background gray90) font-lock-face (:background gray90))))
    (Ok (
      "color 47 with faint" 0 19 (
        face
        (:background gray90 :foreground grey50)
        font-lock-face
        (:background gray90 :foreground grey50))))
    (Ok (
      "color 107" 0 9 (
        face (:background white) font-lock-face (:background white))))
    (Ok (
      "color 107 with faint" 0 20 (
        face
        (:background white :foreground grey50)
        font-lock-face
        (:background white :foreground grey50)))) |}]
;;
