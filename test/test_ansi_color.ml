open! Core_kernel
open! Import
open! Ansi_color

let stock_ansi_color =
  let initialize =
    lazy (
      Feature.require ("ansi-color" |> Symbol.intern);
      let marker_position value =
        if Value.is_marker value
        then value |> Marker.of_value_exn |> Marker.position |> Option.value_exn
        else value |> Position.of_value_exn in
      Symbol.set_value ("ansi-color-apply-face-function" |> Symbol.intern)
        (Function.create [%here]
           ~args:[ "BEGIN" |> Symbol.intern
                 ; "END"   |> Symbol.intern
                 ; "FACE"  |> Symbol.intern ]
           (function [| start; end_; face_spec |] ->
              let face_spec =
                face_spec |> Text.Face_spec.of_value_exn |> Text.Face_spec.normalize in
              if not (List.is_empty face_spec)
              then
                List.iter Text.Property_name.([ face; font_lock_face ]) ~f:(fun property_name ->
                  Current_buffer.set_text_property property_name face_spec
                    ~start:(start |> marker_position)
                    ~end_:( end_  |> marker_position));
              Value.nil
                   | _ -> assert false)
         |> Function.to_value)) in
  fun text ->
    force initialize;
    Current_buffer.set_temporarily_to_temp_buffer (fun () ->
      Point.insert_text text;
      Symbol.funcall2_i Q.ansi_color_apply_on_region
        (Point.min () |> Position.to_value)
        (Point.max () |> Position.to_value);
      Current_buffer.contents ~text_properties:true ())
;;

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
           (foreground (31))
           (italic        false)
           (reverse_video false)
           (underline     false)))
         (text_properties (
           (face ((Face bold) (Attributes ((Foreground (Color red3))))))
           (font-lock-face ((Face bold) (Attributes ((Foreground (Color red3))))))))
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
           (face           ((Face bold)))
           (font-lock-face ((Face bold)))))
         (next_state_by_code ((
           31 (
             (background ())
             (blink_rapid false)
             (blink_slow  false)
             (bold        true)
             (faint       false)
             (foreground (31))
             (italic        false)
             (reverse_video false)
             (underline     false)))))))))) |}];
;;

(* [test] compares [color_text] to [stock_ansi_color] on [string]. *)
let test input =
  let text = input |> Text.of_utf8_bytes in
  let ecaml = try_with (fun () -> color_text text) in
  let stock = try_with (fun () -> stock_ansi_color text) in
  require [%here]
    (match ecaml, stock with
     | Ok ecaml, Ok stock -> Sexp.equal [%sexp (ecaml : Text.t)] [%sexp (stock : Text.t)]
     | _ -> false)
    ~if_false_then_print_s:(
      lazy [%message
        ""
          (input : string)
          (ecaml : Text.t Or_error.t)
          (stock : Text.t Or_error.t)])
;;

let test_codes codes = test (concat [ escape codes; "foo" ])

let%expect_test "empty input" =
  test "";
;;

let%expect_test "no codes" =
  test "foo";
;;

let%expect_test "empty escape sequence" =
  test_codes [];
  [%expect {| |}]
;;

let%expect_test "single code" =
  for code = 0 to max_supported_code do
    test_codes [ code ];
  done;
  [%expect {| |}]
;;

let%expect_test "large code" =
  test_codes [ 123456 ];
  [%expect {| |}]
;;

(* Exhaustive slow tests split out into:
   - test_ansi_color_two_codes.ml
   - test_ansi_color_two_escapes.ml *)

let%expect_test "lots of escape sequences" =
  test (concat (List.init (max_supported_code + 1) ~f:(fun code -> escape [ code ])
                |> List.intersperse ~sep:"z"));
;;

let%expect_test "invalid escape sequences" =
  test "\027";
  test "\027foo";
  test "\027[]foo";
  test "\027[31;40m\027[0mfoo";
  [%expect {| |}]
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
      "#(\"foo\" 0 3 (face (:foreground \"red3\") font-lock-face (:foreground \"red3\")))" |}];
    color_current_buffer () ~start:(Point.min ()) ~end_:(Point.max ());
    show ();
    [%expect {|
      "#(\"foo\" 0 3 (face (:foreground \"red3\") font-lock-face (:foreground \"red3\")))" |}]);
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
      "#(\"foo\" 0 3 (face (:foreground \"red3\") font-lock-face (:foreground \"red3\")))" |}];
    color_current_buffer ();
    show ();
    [%expect {|
      "#(\"foo\" 0 3 (face (:foreground \"red3\") font-lock-face (:foreground \"red3\")))" |}]);
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
  test {|
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
;;

let%expect_test "[Ansi_color_map.get]" =
  print_s [%sexp (Ansi_color_map.get () : Ansi_color_map.t)];
  [%expect {|
    ((Face default)
     (Face bold)
     (Face default)
     (Face italic)
     (Face underline)
     (Face success)
     (Face warning)
     (Face error)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Face nil)
     (Attributes ((Foreground (Color black))))
     (Attributes ((Foreground (Color red3))))
     (Attributes ((Foreground (Color green3))))
     (Attributes ((Foreground (Color yellow3))))
     (Attributes ((Foreground (Color blue2))))
     (Attributes ((Foreground (Color magenta3))))
     (Attributes ((Foreground (Color cyan3))))
     (Attributes ((Foreground (Color gray90))))
     (Face nil)
     (Face nil)
     (Attributes ((Background (Color black))))
     (Attributes ((Background (Color red3))))
     (Attributes ((Background (Color green3))))
     (Attributes ((Background (Color yellow3))))
     (Attributes ((Background (Color blue2))))
     (Attributes ((Background (Color magenta3))))
     (Attributes ((Background (Color cyan3))))
     (Attributes ((Background (Color gray90))))
     (Face nil)
     (Face nil)) |}];
;;
