open! Core_kernel
open! Import
open! Text

module Q = struct
  include Q
  let background_color = "background-color" |> Symbol.intern
  let foreground_color = "foreground-color" |> Symbol.intern
  let format           = "format"           |> Symbol.intern
end

let face           = Property_name.face
let font_lock_face = Property_name.font_lock_face

let%expect_test "[length]" =
  for i = 0 to 3 do
    print_s [%sexp (length (of_utf8_bytes (String.make i 'a')) : int)];
  done;
  [%expect {|
    0
    1
    2
    3 |}];
;;

let%expect_test "[char_code]" =
  let t = of_utf8_bytes "abc" in
  let cs = List.init (length t) ~f:(fun i -> char_code t i) in
  print_s [%sexp (cs : Char_code.t list)];
  [%expect {|
    (97 98 99) |}];
;;

let%expect_test "[set_char_code]" =
  let t = of_utf8_bytes "abc" in
  set_char_code t 1 ('d' |> Char_code.of_char_exn);
  let cs = List.init (length t) ~f:(fun i -> char_code t i) in
  print_s [%sexp (cs : Char_code.t list)];
  [%expect {|
    (97 100 99) |}];
;;

let utf8 = "├" |> of_utf8_bytes

let%expect_test "[num_bytes]" =
  let test t =
    print_s [%message
      ""
        ~length:(length t : int)
        ~num_bytes:(num_bytes t : int)] in
  test ("" |> of_utf8_bytes);
  [%expect {|
    ((length    0)
     (num_bytes 0)) |}];
  test ("foo" |> of_utf8_bytes);
  [%expect {|
    ((length    3)
     (num_bytes 3)) |}];
  test utf8;
  [%expect {|
    ((length    1)
     (num_bytes 3)) |}];
;;

let show_string string =
  let t = string |> of_utf8_bytes in
  print_s
    [%message
      ""
        ~text:(t : t)
        ~length:(length t : int)
        ~num_bytes:(num_bytes t : int)
        ~string:(to_utf8_bytes t : string)]
;;

let%expect_test "ASCII string" =
  show_string "foo";
  [%expect {|
    ((text      foo)
     (length    3)
     (num_bytes 3)
     (string    foo)) |}]
;;

let%expect_test "ordinary UTF-8" =
  show_string "├";
  [%expect {|
    ((text      "\226\148\156")
     (length    1)
     (num_bytes 3)
     (string    "\226\148\156")) |}]
;;

let%expect_test "malformed UTF-8" =
  show_string "\x80\x81\x82";
  [%expect {|
    ((text      "\200\201\202")
     (length    3)
     (num_bytes 6)
     (string    "\128\129\130")) |}]
;;

let show_string_bytes string =
  let n = String.length string in
  printf "length s = %d\n" n;
  for i = 0 to n - 1 do
    printf "s.[ %2d ] = %C\n" i string.[ i ];
  done;
;;

let%expect_test "show steps in the rendering of [text] in the malformed UTF-8 test" =
  let text =
    Symbol.funcall2
      Q.format
      ("%S" |> Value.of_utf8_bytes)
      ("\x80\x81\x82" |> Value.of_utf8_bytes)
    |> of_value_exn in
  let text_bytes = text |> to_utf8_bytes in
  show_string_bytes text_bytes;
  [%expect {|
    length s = 14
    s.[  0 ] = '"'
    s.[  1 ] = '\\'
    s.[  2 ] = '2'
    s.[  3 ] = '0'
    s.[  4 ] = '0'
    s.[  5 ] = '\\'
    s.[  6 ] = '2'
    s.[  7 ] = '0'
    s.[  8 ] = '1'
    s.[  9 ] = '\\'
    s.[ 10 ] = '2'
    s.[ 11 ] = '0'
    s.[ 12 ] = '2'
    s.[ 13 ] = '"' |}];
  let sexp = Sexp.of_string text_bytes in
  let atom =
    match sexp with
    | Atom a -> a
    | _ -> assert false in
  show_string_bytes atom;
  [%expect {|
    length s = 3
    s.[  0 ] = '\200'
    s.[  1 ] = '\201'
    s.[  2 ] = '\202' |}];
  let string = Sexp_pretty.sexp_to_string sexp in
  show_string_bytes string;
  [%expect {|
    length s = 15
    s.[  0 ] = '"'
    s.[  1 ] = '\\'
    s.[  2 ] = '2'
    s.[  3 ] = '0'
    s.[  4 ] = '0'
    s.[  5 ] = '\\'
    s.[  6 ] = '2'
    s.[  7 ] = '0'
    s.[  8 ] = '1'
    s.[  9 ] = '\\'
    s.[ 10 ] = '2'
    s.[ 11 ] = '0'
    s.[ 12 ] = '2'
    s.[ 13 ] = '"'
    s.[ 14 ] = '\n' |}];
  print_s sexp;
  [%expect {|
    "\200\201\202" |}];
;;

let%expect_test "[concat]" =
  let show strings = print_s [%sexp (concat (strings |> List.map ~f:of_utf8_bytes) : t)] in
  show [];
  [%expect {|
    "" |}];
  show [ "a" ];
  [%expect {|
    a |}];
  show [ "a"; "b"; "cde" ];
  [%expect {|
    abcde |}];
;;

let%expect_test "[get_property]" =
  let t = of_utf8_bytes "a" in
  for at = 0 to 2 do
    print_s [%message
      ""
        (at : int)
        ~property:(try_with (fun () -> property_value t ~at face)
                   : Face_spec.t option Or_error.t)];
  done;
  [%expect {|
    ((at 0) (property (Ok ())))
    ((at 1) (property (Ok ())))
    ((at 2) (property (Error (args-out-of-range (2 2))))) |}];
;;

let%expect_test "[set_property]" =
  for start = 0 to 2 do
    for end_ = 0 to 2 do
      print_s [%message (start : int) (end_ : int)];
      let t = of_utf8_bytes "a" in
      match set_property t ~start ~end_ face background_red with
      | () ->
        print_s [%message
          ""
            ~text:(t : t)
            ~property:(try_with (fun () -> property_value t ~at:0 face)
                       : Face_spec.t option Or_error.t)]
      | exception exn -> print_s [%message "failed" ~_:(exn : exn)]
    done;
  done;
  [%expect {|
    ((start 0)
     (end_  0))
    ((text a) (property (Ok ())))
    ((start 0)
     (end_  1))
    ((text (a 0 1 (face (:background red))))
     (property (Ok (((Attributes ((Background (Color red)))))))))
    ((start 0)
     (end_  2))
    (failed (args-out-of-range (0 2)))
    ((start 1)
     (end_  0))
    ((text (a 0 1 (face (:background red))))
     (property (Ok (((Attributes ((Background (Color red)))))))))
    ((start 1)
     (end_  1))
    ((text a) (property (Ok ())))
    ((start 1)
     (end_  2))
    (failed (args-out-of-range (1 2)))
    ((start 2)
     (end_  0))
    (failed (args-out-of-range (0 2)))
    ((start 2)
     (end_  1))
    (failed (args-out-of-range (1 2)))
    ((start 2)
     (end_  2))
    ((text a) (property (Ok ()))) |}]
;;

let face_spec_ones : Face_spec.One.t list =
  [ Attributes [ T (Slant, Italic) ]
  ; Attributes [ T (Weight, Bold) ]
  ; Face Face.default ]
;;

let%expect_test "various [Face_spec.t] values" =
  let property_name = face in
  let t = of_utf8_bytes "z" in
  let check face_spec =
    print_s [%sexp (face_spec : Face_spec.t)];
    set_property t property_name face_spec;
    let round_trip = property_value t property_name ~at:0 |> Option.value_exn in
    require [%here]
      (Value.equal
         (face_spec |> Face_spec.to_value)
         (round_trip |> Face_spec.to_value))
      ~if_false_then_print_s:(lazy [%message (round_trip : Face_spec.t)]) in
  List.iter face_spec_ones ~f:(fun f1 ->
    check [ f1 ];
    List.iter face_spec_ones ~f:(fun f2 ->
      check [ f1; f2 ]));
  [%expect {|
    ((Attributes ((Slant Italic))))
    ((Attributes ((Slant Italic)))
     (Attributes ((Slant Italic))))
    ((Attributes ((Slant  Italic)))
     (Attributes ((Weight Bold))))
    ((Attributes ((Slant Italic))) (Face default))
    ((Attributes ((Weight Bold))))
    ((Attributes ((Weight Bold)))
     (Attributes ((Slant  Italic))))
    ((Attributes ((Weight Bold)))
     (Attributes ((Weight Bold))))
    ((Attributes ((Weight Bold))) (Face default))
    ((Face default))
    ((Face default) (Attributes ((Slant Italic))))
    ((Face default) (Attributes ((Weight Bold))))
    ((Face default)
     (Face default)) |}];
;;

let%expect_test "deprecated face specs" =
  let test value = print_s [%sexp (Face_spec.of_value_exn value : Face_spec.t)] in
  List.iter
    [ Q.background_color
    ; Q.foreground_color ]
    ~f:(fun q ->
      let spec =
        Value.cons
          (q |> Symbol.to_value)
          (Color.red |> Color.to_value) in
      test spec;
      List.iter face_spec_ones ~f:(fun one ->
        test (Value.list [ spec; one |> Face_spec.One.to_value ]);
        test (Value.list [ one |> Face_spec.One.to_value; spec ])));
  [%expect {|
    ((Attributes ((Background (Color red)))))
    ((Attributes ((Background (Color red)))) (Attributes ((Slant Italic))))
    ((Attributes ((Slant Italic))) (Attributes ((Background (Color red)))))
    ((Attributes ((Background (Color red)))) (Attributes ((Weight Bold))))
    ((Attributes ((Weight Bold))) (Attributes ((Background (Color red)))))
    ((Attributes ((Background (Color red)))) (Face default))
    ((Face default) (Attributes ((Background (Color red)))))
    ((Attributes ((Foreground (Color red)))))
    ((Attributes ((Foreground (Color red)))) (Attributes ((Slant Italic))))
    ((Attributes ((Slant Italic))) (Attributes ((Foreground (Color red)))))
    ((Attributes ((Foreground (Color red)))) (Attributes ((Weight Bold))))
    ((Attributes ((Weight Bold))) (Attributes ((Foreground (Color red)))))
    ((Attributes ((Foreground (Color red)))) (Face default))
    ((Face default) (Attributes ((Foreground (Color red))))) |}];
;;

let%expect_test "[add_properties]" =
  let t = of_utf8_bytes "a" in
  add_properties t [ T (face, background_red)];
  print_s [%sexp (t : t)];
  [%expect {| (a 0 1 (face (:background red))) |}];
  add_properties t [ T (face, background_blue)];
  print_s [%sexp (t : t)];
  [%expect {| (a 0 1 (face (:background blue))) |}];
  add_properties t [ T (font_lock_face, background_red)];
  print_s [%sexp (t : t)];
  [%expect {| (a 0 1 (face (:background blue) font-lock-face (:background red))) |}];
;;

let%expect_test "[set_properties]" =
  let t = of_utf8_bytes "a" in
  set_properties t [ T (face, background_red)];
  print_s [%sexp (t : t)];
  [%expect {|
    (a 0 1 (face (:background red))) |}];
  set_properties t [ T (face, background_blue)];
  print_s [%sexp (t : t)];
  [%expect {|
    (a 0 1 (face (:background blue))) |}];
  set_properties t [ T (font_lock_face, background_red)];
  print_s [%sexp (t : t)];
  [%expect {|
    (a 0 1 (font-lock-face (:background red))) |}];
;;

let%expect_test "[properties]" =
  let t = of_utf8_bytes "a" in
  let show () = print_s [%sexp (properties t ~at:0 : Property.t list)] in
  show ();
  [%expect {|
    () |}];
  set_properties t [ T (face, background_blue)
                   ; T (font_lock_face, background_blue) ];
  show ();
  [%expect {|
    ((face ((Attributes ((Background (Color blue))))))
     (font-lock-face ((Attributes ((Background (Color blue))))))) |}];
;;

let%expect_test "registering a new text property" =
  let property_name =
    Property_name.create_and_register (module struct
      let name = "foo" |> Symbol.intern
      module Property_value = Symbol
    end) in
  let t = of_utf8_bytes "a" in
  set_property t property_name ("bar" |> Symbol.intern);
  print_s [%sexp (t : t)];
  [%expect {|
    (a 0 1 (foo bar)) |}];
  print_s [%sexp (property_value t ~at:0 property_name : Symbol.t option)];
  [%expect {|
    (bar) |}];
  print_s [%sexp (properties t ~at:0 : Property.t list)];
  [%expect {|
    ((foo bar)) |}];
;;

let%expect_test "[remove_properties]" =
  let t = of_utf8_bytes "a" in
  remove_properties t [ T face ];
  set_properties t [ T (face          , background_red)
                   ; T (font_lock_face, background_red)];
  print_s [%sexp (t : t)];
  [%expect {|
    (a 0 1 (font-lock-face (:background red) face (:background red))) |}];
  remove_properties t [ T font_lock_face ];
  print_s [%sexp (t : t)];
  [%expect {|
    (a 0 1 (face (:background red))) |}];
  remove_properties t [ T face ];
  print_s [%sexp (t : t)];
  [%expect {|
    a |}];
;;

let%expect_test "[propertize]" =
  let test properties =
    let t = propertize ("foo" |> of_utf8_bytes) properties in
    print_s [%sexp (t : t)] in
  test [];
  [%expect {|
    foo |}];
  test [ T (face, []) ];
  [%expect {|
    (foo 0 3 (face nil)) |}];
  test [ T (face, background_red) ];
  [%expect {|
    (foo 0 3 (face (:background red))) |}];
  test [ T (face, [ Attributes [ T (Background , Color Color.red)
                               ; T (Foreground , Color Color.green)
                               ; T (Slant      , Italic)
                               ; T (Weight     , Bold)]])];
  [%expect {|
    (foo 0 3 (face (:background red :foreground green :slant italic :weight bold))) |}];
;;

let%expect_test "[is_multibyte], [to_multibyte], [to_unibyte]" =
  let test t = print_s [%sexp (is_multibyte t : bool)] in
  test ("" |> of_utf8_bytes);
  [%expect {|
    true |}];
  test ("" |> of_utf8_bytes |> to_unibyte_exn);
  [%expect {|
    false |}];
  test ("" |> of_utf8_bytes |> to_unibyte_exn |> to_multibyte);
  [%expect {|
    true |}];
  test utf8;
  [%expect {|
    true |}];
;;

let%expect_test "[to_unibyte_exn] raise" =
  require_does_raise [%here] (fun () -> to_unibyte_exn utf8);
  [%expect {|
    ("Can't convert the 0th character to unibyte") |}];
;;
