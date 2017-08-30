open! Core_kernel
open! Import
open! Current_buffer

let show () = print_s [%sexp (get () : Buffer.t)]

let show_point () = print_s [%message "" ~point:(Point.get () : Position.t)]

let%expect_test "enable transient-mark-mode for the duration of this file" =
  print_s [%sexp (Marker.transient_mark_mode_is_enabled () : bool)];
  [%expect {|
    false |}];
  Marker.set_transient_mark_mode true;
  print_s [%sexp (Marker.transient_mark_mode_is_enabled () : bool)];
  [%expect {|
    true |}];
;;

let%expect_test "[get]" =
  show ();
  [%expect {|
    "#<buffer *scratch*>" |}];
;;

let%expect_test "[set]" =
  let old = get () in
  let t = Buffer.create ~name:"foo" in
  set t;
  show ();
  [%expect {|
    "#<buffer foo>" |}];
  set old;
  show ();
  [%expect {|
    "#<buffer *scratch*>" |}];
  Buffer.kill t;
;;

let%expect_test "[set_temporarily]" =
  let t = Buffer.create ~name:"foo" in
  set_temporarily t ~f:(fun () ->
    set t;
    show ();
    [%expect {|
    "#<buffer foo>" |}]);
  show ();
  [%expect {|
    "#<buffer *scratch*>" |}];
  Buffer.kill t;
;;

let%expect_test "[set_temporarily] when [~f] raises" =
  let t = Buffer.create ~name:"foo" in
  show_raise ~hide_positions:true (fun () ->
    set_temporarily t ~f:(fun () -> raise_s [%message [%here]]));
  [%expect {|
    (raised app/emacs/lib/ecaml/test/test_current_buffer.ml:LINE:COL) |}];
  show ();
  [%expect {|
    "#<buffer *scratch*>" |}];
  Buffer.kill t;
;;

let%expect_test "[set_temporarily_to_temp_buffer]" =
  set_temporarily_to_temp_buffer (fun () -> show ());
  [%expect {|
    "#<buffer *temp-buffer*>" |}];
  show ();
  [%expect {|
    "#<buffer *scratch*>" |}];
;;

let%expect_test "[is_modified], [set_modified]" =
  set_temporarily_to_temp_buffer (fun () ->
    print_s [%sexp (is_modified () : bool)];
    [%expect {|
      false |}];
    Point.insert "foo";
    print_s [%sexp (is_modified () : bool)];
    [%expect {|
      true |}];
    set_modified false;
    print_s [%sexp (is_modified () : bool)];
    [%expect {|
      false |}]);
;;

let%expect_test "[contents]" =
  set_temporarily_to_temp_buffer (fun () ->
    print_s [%sexp (contents () : Text.t)];
    [%expect {|
      "" |}];
    Point.insert "foo";
    print_s [%sexp (contents () : Text.t)];
    [%expect {|
      foo |}];
    print_s [%sexp (contents ()
                      ~start:(1 |> Position.of_int_exn)
                      ~end_:( 2 |> Position.of_int_exn)
                    : Text.t)];
    [%expect {|
      f |}]);
;;

let%expect_test "[contents_text ~text_properties:true], [insert_text]" =
  set_temporarily_to_temp_buffer (fun () ->
    let show () = print_s [%sexp (contents ~text_properties:true () : Text.t)] in
    show ();
    [%expect {|
      "" |}];
    Point.insert_text (Text.propertize (Text.of_utf8_bytes "foo")
                         [ T (Text.Property_name.face, background_red) ]);
    show ();
    [%expect {|
       "#(\"foo\" 0 3 (face (:background \"red\")))" |}];
    print_s [%sexp (contents ()
                      ~start:(1 |> Position.of_int_exn)
                      ~end_:( 2 |> Position.of_int_exn)
                      ~text_properties:true
                    : Text.t)];
    [%expect {|
      "#(\"f\" 0 1 (face (:background \"red\")))" |}]);
;;

let%expect_test "[kill]" =
  let buffer = Buffer.create ~name:"z" in
  let show_live () = print_s [%sexp (Buffer.is_live buffer : bool)] in
  show_live ();
  [%expect {|
    true |}];
  set buffer;
  kill ();
  show_live ();
  [%expect {|
    false |}];
;;

let%expect_test "[save]" =
  let file = Caml.Filename.temp_file "" "" in
  Selected_window.find_file file;
  Point.insert "foobar";
  save ();
  kill ();
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert_file_contents_exn file;
    print_s [%sexp (contents () : Text.t)];
    [%expect {|
      foobar |}]);
  Sys.remove file;
;;

let%expect_test "[erase]" =
  set_temporarily_to_temp_buffer (fun () ->
    erase ();
    Point.insert "foo";
    erase ();
    print_s [%sexp (contents () : Text.t)];
    [%expect {|
      "" |}]);
;;

let%expect_test "[kill_region]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foobar";
    kill_region
      ~start:(2 |> Position.of_int_exn)
      ~end_:( 4  |> Position.of_int_exn);
    print_s [%sexp (contents () : Text.t)];
    [%expect {|
      fbar |}]);
;;

let%expect_test "[save_excursion]" =
  let i = save_excursion (fun () -> 13) in
  print_s [%sexp (i : int)];
  [%expect {|
    13 |}];
;;

let%expect_test "[save_excursion] preserves point" =
  set_temporarily_to_temp_buffer (fun () ->
    show_point ();
    save_excursion (fun () ->
      Point.insert "foo";
      show_point ());
    show_point ());
  [%expect {|
    (point 1)
    (point 4)
    (point 1) |}];
;;

let%expect_test "[save_excursion] preserves current buffer" =
  let t = Buffer.find_or_create ~name:"zzz" in
  save_excursion (fun () -> set t; show ());
  show ();
  [%expect {|
    "#<buffer zzz>"
    "#<buffer *scratch*>" |}];
;;

let%expect_test "[set_multibyte], [is_multibyte]" =
  let show () = print_s [%message "" ~is_multibyte:(is_multibyte () : bool)] in
  show ();
  [%expect {|
    (is_multibyte true) |}];
  set_multibyte false;
  show ();
  [%expect {|
    (is_multibyte false) |}];
  set_multibyte true;
  show ();
  [%expect {|
    (is_multibyte true) |}];
;;

let%expect_test "[rename_exn]" =
  let t = Buffer.create ~name:"foo" in
  set_temporarily t ~f:(fun () ->
    rename_exn () ~name:"bar";
    show ());
  [%expect {|
    "#<buffer bar>" |}];
  Buffer.kill t;
;;

let%expect_test "[rename_exn] raise" =
  let t1 = Buffer.create ~name:"foo" in
  let t2 = Buffer.create ~name:"bar" in
  require_does_raise [%here] (fun () ->
    set_temporarily t1 ~f:(fun () ->
      rename_exn () ~name:(Buffer.name t2 |> Option.value_exn)));
  [%expect {|
    (signal (symbol error) (data ("Buffer name `bar' is in use"))) |}];
  Buffer.kill t1;
  Buffer.kill t2;
;;

let%expect_test "[rename_exn ~unique:true]" =
  let t1 = Buffer.create ~name:"foo" in
  set_temporarily t1 ~f:(fun () ->
    rename_exn () ~name:"bar" ~unique:true;
    show ());
  [%expect {|
    "#<buffer bar>" |}];
  let t2 = Buffer.create ~name:"foo" in
  set_temporarily t2 ~f:(fun () ->
    rename_exn () ~name:"bar" ~unique:true;
    show ());
  [%expect {|
    "#<buffer bar<2>>" |}];
  Buffer.kill t1;
  Buffer.kill t2;
;;

let%expect_test "[text_property_is_present]" =
  set_temporarily_to_temp_buffer (fun () ->
    let show () =
      print_s [%sexp (text_property_is_present Text.Property_name.face : bool)] in
    show ();
    [%expect {|
      false |}];
    let insert property_name =
      Point.insert_text
        (Text.propertize (Text.of_utf8_bytes "foo")
           [ T (property_name, background_red)]) in
    insert Text.Property_name.font_lock_face;
    show ();
    [%expect {|
      false |}];
    insert Text.Property_name.face;
    show ();
    [%expect {|
      true |}]);
;;

let%expect_test "[is_read_only], [set_read_only]" =
  set_temporarily_to_temp_buffer (fun () ->
    let show () = print_s [%message "" ~is_read_only:(is_read_only () : bool)] in
    show ();
    [%expect {|
      (is_read_only false) |}];
    set_read_only true;
    show ();
    [%expect {|
      (is_read_only true) |}];
    set_read_only false;
    show ();
    [%expect {|
      (is_read_only false) |}]);
;;

let face           = Text.Property_name.face
let font_lock_face = Text.Property_name.font_lock_face

let show () = print_s [%sexp (contents ~text_properties:true () : Text.t)]

let%expect_test "[set_text_property]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    set_text_property face foreground_red;
    show ();
    [%expect {|
      "#(\"foo\" 0 3 (face (:foreground \"red\")))" |}];
    set_text_property face foreground_blue;
    show ();
    [%expect {|
      "#(\"foo\" 0 3 (face (:foreground \"blue\")))" |}]);
;;

let%expect_test "[set_text_property_staged]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    let set = unstage (set_text_property_staged face foreground_red) in
    set ~start:1 ~end_:2;
    set ~start:2 ~end_:3;
    show ();
    [%expect {|
      "#(\"foo\" 0 1 (face (:foreground \"red\")) 1 2 (face (:foreground \"red\")))" |}]);
;;

let%expect_test "[set_text_properties]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    set_text_properties [ T (face, foreground_red) ];
    show ();
    [%expect {|
      "#(\"foo\" 0 3 (face (:foreground \"red\")))" |}];
    set_text_properties [ T (font_lock_face, foreground_red) ];
    show ();
    [%expect {|
      "#(\"foo\" 0 3 (font-lock-face (:foreground \"red\")))" |}]);
;;

let%expect_test "[set_text_properties_staged]" =
  set_temporarily_to_temp_buffer (fun () ->
    let show () = print_s [%sexp (contents ~text_properties:true () : Text.t)] in
    Point.insert "foo";
    let set = unstage (set_text_properties_staged [ T (face, foreground_red) ]) in
    set ~start:1 ~end_:2;
    set ~start:2 ~end_:3;
    show ();
    [%expect {|
      "#(\"foo\" 0 1 (face (:foreground \"red\")) 1 2 (face (:foreground \"red\")))" |}];
    set_text_property face foreground_blue;
    set ~start:2 ~end_:3;
    show ();
    [%expect {|
      "#(\"foo\" 0 1 (face (:foreground \"blue\")) 1 2 (face (:foreground \"red\")) 2 3 (face (:foreground \"blue\")))" |}]);
;;

let%expect_test "[add_text_properties]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    add_text_properties [ T (face, foreground_red) ];
    show ();
    [%expect {|
      "#(\"foo\" 0 3 (face (:foreground \"red\")))" |}];
    add_text_properties [ T (font_lock_face, foreground_red) ];
    show ();
    [%expect {|
      "#(\"foo\" 0 3 (face (:foreground \"red\") font-lock-face (:foreground \"red\")))" |}]);
;;

let%expect_test "[add_text_properties_staged]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    set_text_property font_lock_face foreground_blue;
    let add = unstage (add_text_properties_staged [ T (face, foreground_red) ]) in
    add ~start:1 ~end_:2;
    add ~start:2 ~end_:3;
    show ();
    [%expect {|
      "#(\"foo\" 0 1 (font-lock-face (:foreground \"blue\") face (:foreground \"red\")) 1 2 (font-lock-face (:foreground \"blue\") face (:foreground \"red\")) 2 3 (font-lock-face (:foreground \"blue\")))" |}]);
;;

let%expect_test "[is_undo_enabled] [set_undo_enabled]" =
  set_temporarily_to_temp_buffer (fun () ->
    let show () =
      print_s [%message
        ""
          ~is_undo_enabled:(is_undo_enabled () : bool)] in
    show ();
    [%expect {|
      (is_undo_enabled true) |}];
    set_undo_enabled false;
    show ();
    [%expect {|
      (is_undo_enabled false) |}];
    set_undo_enabled true;
    show ();
    [%expect {|
      (is_undo_enabled true) |}]);
;;

let%expect_test "[undo], [add_undo_boundary]" =
  let restore_stderr = unstage (ignore_stderr ()) in
  set_temporarily_to_temp_buffer (fun () ->
    let show () =
      print_s [%message
        ""
          ~contents:(contents () : Text.t)
          ~undo_list:(undo_list () : Value.t)] in
    show ();
    [%expect {|
      ((contents  "")
       (undo_list nil)) |}];
    add_undo_boundary ();
    show ();
    [%expect {|
      ((contents  "")
       (undo_list nil)) |}];
    Point.insert "foo";
    show ();
    [%expect {|
      ((contents foo)
       (undo_list (
         (1 . 4)
         (t . 0)))) |}];
    add_undo_boundary ();
    show ();
    [%expect {|
      ((contents foo)
       (undo_list (
         nil
         (1 . 4)
         (t . 0)))) |}];
    undo 1;
    show ();
    [%expect {|
      ((contents "")
       (undo_list (
         (foo . 1)
         nil
         (1 . 4)
         (t . 0)))) |}];
    Point.insert "bar";
    add_undo_boundary ();
    Point.insert "baz";
    show ();
    [%expect {|
      ((contents barbaz)
       (undo_list (
         (4 . 7)
         nil
         (1   . 4)
         (t   . 0)
         (foo . 1)
         nil
         (1 . 4)
         (t . 0)))) |}];
    undo 2;
    show ();
    [%expect {|
      ((contents "")
       (undo_list (
         (foo . 1)
         (1   . 4)
         (t   . 0)
         (bar . 1)
         (baz . 4)
         (4   . 7)
         nil
         (1   . 4)
         (t   . 0)
         (foo . 1)
         nil
         (1 . 4)
         (t . 0)))) |}]);
  restore_stderr ();
;;

let%expect_test "[get_mark], [set_mark]" =
  let show () = print_s [%sexp (get_mark () : Marker.t)] in
  set_temporarily_to_temp_buffer (fun () ->
    show ();
    [%expect {|
      "#<marker in no buffer>" |}];
    Point.insert "foo";
    set_mark (2 |> Position.of_int_exn);
    show ();
    [%expect {| "#<marker at 2 in *temp-buffer*>" |}]);
;;

let%expect_test "[mark_is_active], [deactivate_mark]" =
  set_temporarily_to_temp_buffer (fun () ->
    let show () = print_s [%sexp (mark_is_active () : bool)] in
    show ();
    [%expect {|
      false |}];
    set_mark (1 |> Position.of_int_exn);
    require [%here] (mark_is_active ());
    show ();
    [%expect {|
      true |}];
    deactivate_mark ();
    require [%here] (not (mark_is_active ()));
    show ();
    [%expect {|
      false |}]);
;;

let%expect_test "[set_marker_position]" =
  set_temporarily_to_temp_buffer (fun () ->
    let marker = Marker.create () in
    let set i =
      set_marker_position marker (i |> Position.of_int_exn);
      print_s [%sexp (marker : Marker.t)] in
    set 1;
    [%expect {|
      "#<marker at 1 in *temp-buffer*>" |}];
    set 2;
    [%expect {|
      "#<marker at 1 in *temp-buffer*>" |}];
    Point.insert "foo";
    set 2;
    [%expect {|
      "#<marker at 2 in *temp-buffer*>" |}]);
;;

let%expect_test "[Minor_mode.is_enabled], [Minor_mode.enable], [Minor_mode.disable]" =
  let is_enabled () = print_s [%sexp (Minor_mode.(is_enabled view)  : bool)] in
  is_enabled ();
  [%expect {|
    false |}];
  Minor_mode.(enable view);
  is_enabled ();
  [%expect {|
    true |}];
  Minor_mode.(disable view);
  is_enabled ();
  [%expect {|
    false |}];
;;

let%expect_test "[Minor_mode.read_only]" =
  let is_enabled () = print_s [%sexp (Minor_mode.(is_enabled read_only)  : bool)] in
  is_enabled ();
  [%expect {|
    false |}];
  Minor_mode.(enable read_only);
  is_enabled ();
  [%expect {|
    true |}];
  Minor_mode.(disable read_only);
  is_enabled ();
  [%expect {|
    false |}];
;;
