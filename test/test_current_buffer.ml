open! Core_kernel
open! Import
open! Current_buffer

let show () = print_s [%sexp (get () : Buffer.t)]

let show_point () = print_s [%message "" ~point:(Point.get () : Position.t)]

let%expect_test "[set_transient_mark_mode], [transient_mark_mode_is_enabled]" =
  let show () = print_s [%sexp (value_exn transient_mark_mode : bool)] in
  show ();
  [%expect {|
    false |}];
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    show ();
    [%expect {|
      false |}];
    Current_buffer.set_temporarily_to_temp_buffer (fun () ->
      show ();
      [%expect {|
        false |}];
      set_value transient_mark_mode true;
      show ();
      [%expect {|
        true |}]);
    show ();
    [%expect {|
      true |}]);
  show ();
  [%expect {|
    true |}];
;;

let%expect_test "enable transient-mark-mode for the duration of this file" =
  set_value transient_mark_mode true;
  print_s [%sexp (value_exn transient_mark_mode : bool)];
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

let%expect_test "[value_exn] raise" =
  let var = int_var "z" in
  show_raise (fun () -> value_exn var);
  [%expect {|
    (raised ("[Current_buffer.value_exn] of undefined variable" (z int))) |}];
;;

let%expect_test "[value] of unbound symbol" =
  print_s [%sexp (value (int_var "z") : int option)];
  [%expect {|
    () |}];
;;

let%expect_test "[clear_value]" =
  let var = int_var "z" in
  let show () = print_s [%sexp (value var : int option)] in
  show ();
  [%expect {|
    () |}];
  set_value var 13;
  show ();
  [%expect {|
    (13) |}];
  clear_value var;
  show ();
  [%expect {|
    () |}];
;;

let%expect_test "[set_value], [value_exn]" =
  let var = int_var "z" in
  set_value var 13;
  print_s [%sexp (value_exn var : int)];
  [%expect {|
    13 |}];
;;

let%expect_test "[has_non_null_value]" =
  let var = Var.create (Symbol.create ~name:"zzz") Value.Type.value in
  let show () = print_s [%sexp (has_non_null_value var : bool)] in
  show ();
  [%expect {|
    false |}];
  set_value var Value.nil;
  show ();
  [%expect {|
    false |}];
  set_value var Value.t;
  show ();
  [%expect {|
    true |}];
  set_value var (13 |> Value.of_int_exn);
  show ();
  [%expect {|
    true |}];
;;

let%expect_test "[set_value_temporarily]" =
  let var = int_var "z" in
  let show () = print_s [%sexp (value_exn var : int)] in
  set_value var 13;
  show ();
  [%expect {|
    13 |}];
  set_value_temporarily var 14 ~f:(fun () ->
    show ();
    [%expect {|
      14 |}]);
  show ();
  [%expect {|
    13 |}];
;;

let%expect_test "[set_value_temporarily] with an unbound var" =
  let var = int_var "z" in
  let show () = print_s [%sexp (value var : int option)] in
  show ();
  [%expect {|
    () |}];
  set_value_temporarily var 13 ~f:(fun () ->
    show ();
    [%expect {|
      (13) |}]);
  show ();
  [%expect {|
    () |}];
;;

let%expect_test "[set_value_temporarily] with a buffer-local and changed buffer" =
  let var = Var.create (Symbol.create ~name:"z") Value.Type.(option int) in
  Var.make_buffer_local_always var;
  let b1 = Buffer.create ~name:"b1" in
  let b2 = Buffer.create ~name:"b2" in
  let value_in buffer = Buffer.buffer_local_value buffer var in
  let show_values () =
    print_s [%message "" ~b1:(value_in b1 : int option) ~b2:(value_in b2 : int option)] in
  show_values ();
  [%expect {|
    ((b1 ())
     (b2 ())) |}];
  Current_buffer.set_temporarily b1 ~f:(fun () ->
    set_value var (Some 13);
    show_values ();
    [%expect {|
      ((b1 (13)) (b2 ())) |}];
    set_value_temporarily var (Some 14) ~f:(fun () ->
      show_values ();
      [%expect {|
        ((b1 (14)) (b2 ())) |}];
      Current_buffer.set b2);
    show_values ();
    [%expect {|
      ((b1 (13)) (b2 ())) |}]);
;;

let%expect_test "[set_values_temporarily]" =
  let v1 = int_var "z1" in
  let v2 = int_var "z2" in
  let show () =
    print_s [%message
      ""
        ~_:(value_exn v1 : int)
        ~_:(value_exn v2 : int)] in
  set_value v1 13;
  set_value v2 14;
  show ();
  [%expect {|
    (13 14) |}];
  set_values_temporarily [ T (v1, 15); T (v2, 16) ] ~f:(fun () ->
    show ();
    [%expect {|
      (15 16) |}]);
  show ();
  [%expect {|
    (13 14) |}];
;;

let%expect_test "[file_name]" =
  let show_file_basename () =
    print_s [%sexp (file_name () |> Option.map ~f:Filename.nondirectory
                    : string option)] in
  show_file_basename ();
  [%expect {|
    () |}];
  Selected_window.find_file "test_current_buffer.ml";
  show_file_basename ();
  [%expect {|
    (test_current_buffer.ml) |}];
  kill ();
;;

let%expect_test "[file_name_exn] raise" =
  set_temporarily_to_temp_buffer (fun () ->
    show_raise (fun () -> file_name_exn ()));
  [%expect {|
    (raised ("buffer does not have a file name" "#<buffer *temp-buffer*>")) |}];
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
       (foo 0 3 (face (:background red))) |}];
    print_s [%sexp (contents ()
                      ~start:(1 |> Position.of_int_exn)
                      ~end_:( 2 |> Position.of_int_exn)
                      ~text_properties:true
                    : Text.t)];
    [%expect {|
      (f 0 1 (face (:background red))) |}]);
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
    ("Buffer name `bar' is in use") |}];
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
    let show () = print_s [%message "" ~is_read_only:(value_exn read_only : bool)] in
    show ();
    [%expect {|
      (is_read_only false) |}];
    set_value read_only true;
    show ();
    [%expect {|
      (is_read_only true) |}];
    set_value read_only false;
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
      (foo 0 3 (face (:foreground red))) |}];
    set_text_property face foreground_blue;
    show ();
    [%expect {|
      (foo 0 3 (face (:foreground blue))) |}]);
;;

let%expect_test "[set_text_property_staged]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    let set = unstage (set_text_property_staged face foreground_red) in
    set ~start:1 ~end_:2;
    set ~start:2 ~end_:3;
    show ();
    [%expect {|
      (foo 0 1 (face (:foreground red)) 1 2 (face (:foreground red))) |}]);
;;

let%expect_test "[set_text_properties]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    set_text_properties [ T (face, foreground_red) ];
    show ();
    [%expect {|
      (foo 0 3 (face (:foreground red))) |}];
    set_text_properties [ T (font_lock_face, foreground_red) ];
    show ();
    [%expect {|
      (foo 0 3 (font-lock-face (:foreground red))) |}]);
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
      (foo 0 1 (face (:foreground red)) 1 2 (face (:foreground red))) |}];
    set_text_property face foreground_blue;
    set ~start:2 ~end_:3;
    show ();
    [%expect {|
      (foo 0 1
        (face (:foreground blue))
        1
        2
        (face (:foreground red))
        2
        3
        (face (:foreground blue))) |}]);
;;

let%expect_test "[add_text_properties]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "foo";
    add_text_properties [ T (face, foreground_red) ];
    show ();
    [%expect {|
      (foo 0 3 (face (:foreground red))) |}];
    add_text_properties [ T (font_lock_face, foreground_red) ];
    show ();
    [%expect {|
      (foo 0 3 (face (:foreground red) font-lock-face (:foreground red))) |}]);
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
      (foo 0 1
        (font-lock-face (:foreground blue) face (:foreground red))
        1
        2
        (font-lock-face (:foreground blue) face (:foreground red))
        2
        3
        (font-lock-face (:foreground blue))) |}]);
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

let%expect_test "[mark], [set_mark]" =
  let show () = print_s [%sexp (mark () : Marker.t)] in
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

let show_var var =
  print_s [%message
    ""
      ~value:(Current_buffer.value { var with type_ = Value.Type.value } : Value.t option)
      ~is_buffer_local:(is_buffer_local var : bool)
      ~is_buffer_local_if_set:(is_buffer_local_if_set var : bool)]
;;

let%expect_test "[make_buffer_local], [is_buffer_local], [is_buffer_local_if_set]" =
  let var = int_var "s" in
  set_temporarily_to_temp_buffer (fun () ->
    make_buffer_local var;
    Current_buffer.set_value var 13;
    show_var var;
    [%expect {|
      ((value (13))
       (is_buffer_local        true)
       (is_buffer_local_if_set true)) |}]);
  show_var var;
  [%expect {|
    ((value ())
     (is_buffer_local        false)
     (is_buffer_local_if_set false)) |}];
;;

let%expect_test "[kill_buffer_local]" =
  let var = int_var "z" in
  Current_buffer.set_value var 13;
  set_temporarily_to_temp_buffer (fun () ->
    make_buffer_local var;
    Current_buffer.set_value var 14;
    show_var var;
    [%expect {|
      ((value (14))
       (is_buffer_local        true)
       (is_buffer_local_if_set true)) |}];
    kill_buffer_local var;
    show_var var;
    [%expect {|
      ((value (13))
       (is_buffer_local        false)
       (is_buffer_local_if_set false)) |}]);
;;

let%expect_test "[local_keymap], [set_local_keymap]" =
  let show_local_keymap () = print_s [%sexp (local_keymap () : Keymap.t option)] in
  set_temporarily_to_temp_buffer (fun () ->
    show_local_keymap ();
    [%expect {|
      () |}];
    let keymap = Keymap.create () in
    Keymap.define_key keymap ("a" |> Key_sequence.create_exn) Undefined;
    set_local_keymap keymap;
    show_local_keymap ();
    [%expect {|
      ((keymap (97 . undefined))) |}]);
;;

let%expect_test "[minor_mode_keymaps]" =
  set_temporarily_to_temp_buffer (fun () ->
    Minor_mode.(enable view);
    print_s [%sexp (minor_mode_keymaps () : Keymap.t list)];
    [%expect {|
      ((
        keymap
        (104      . describe-mode)
        (63       . describe-mode)
        (72       . describe-mode)
        (48       . digit-argument)
        (49       . digit-argument)
        (50       . digit-argument)
        (51       . digit-argument)
        (52       . digit-argument)
        (53       . digit-argument)
        (54       . digit-argument)
        (55       . digit-argument)
        (56       . digit-argument)
        (57       . digit-argument)
        (45       . negative-argument)
        (60       . beginning-of-buffer)
        (62       . end-of-buffer)
        (111      . View-scroll-to-buffer-end)
        (33554464 . View-scroll-page-backward)
        (32       . View-scroll-page-forward)
        (127      . View-scroll-page-backward)
        (119      . View-scroll-page-backward-set-page-size)
        (122      . View-scroll-page-forward-set-page-size)
        (100      . View-scroll-half-page-forward)
        (117      . View-scroll-half-page-backward)
        (13       . View-scroll-line-forward)
        (10       . View-scroll-line-forward)
        (121      . View-scroll-line-backward)
        (70       . View-revert-buffer-scroll-page-forward)
        (61       . what-line)
        (103      . View-goto-line)
        (37       . View-goto-percent)
        (46       . set-mark-command)
        (64       . View-back-to-mark)
        (120      . exchange-point-and-mark)
        (39       . register-to-point)
        (109      . point-to-register)
        (115      . isearch-forward)
        (114      . isearch-backward)
        (47       . View-search-regexp-forward)
        (92       . View-search-regexp-backward)
        (110      . View-search-last-regexp-forward)
        (112      . View-search-last-regexp-backward)
        (113      . View-quit)
        (101      . View-exit)
        (69       . View-exit-and-edit)
        (81       . View-quit-all)
        (99       . View-leave)
        (67       . View-kill-and-leave))) |}]);
;;

let%expect_test "[delete_duplicate_lines]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "\
a
a
b
b
c
a
";
    delete_duplicate_lines ();
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      a
      b
      c |}]);
;;

let%expect_test "[delete_lines_matching]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "\
a
a
b
b
c
a
";
    delete_lines_matching ("b" |> Regexp.quote);
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      a
      a
      c
      a |}]);
;;

let%expect_test "[sort_lines]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "\
a
d
b
c
";
    sort_lines ();
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      a
      b
      c
      d |}]);
;;

let%expect_test "[delete_region]" =
  set_temporarily_to_temp_buffer (fun () ->
    Point.insert "123456789";
    delete_region
      ~start:(3 |> Position.of_int_exn)
      ~end_: (5 |> Position.of_int_exn);
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      1256789 |}]);
;;

let%expect_test "[indent_region]" =
  set_temporarily_to_temp_buffer (fun () ->
    Symbol.funcall0_i ("c-mode" |> Symbol.intern);
    Point.insert "\
      void f () {
      foo;
      bar;
      }";
    indent_region ();
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      void f () {
        foo;
        bar;
      } |}]);
;;

let%expect_test "[revert]" =
  let dir = "dir.tmp" in
  Directory.create dir ~parents:true;
  Selected_window.find_file (concat [ dir; "/file" ]);
  ignore (Process.shell_command_exn "echo foo >file" ~working_directory:Of_current_buffer
          : string);
  print_s [%sexp (contents () : Text.t)];
  [%expect {| "" |}];
  revert ();
  print_s [%sexp (contents () : Text.t)];
  [%expect {| "foo\n" |}];
  kill ();
  Directory.delete dir ~recursive:true;
;;

let%expect_test "[set_revert_buffer_function]" =
  set_temporarily_to_temp_buffer (fun () ->
    set_revert_buffer_function (fun ~confirm ->
      print_s [%message "called" (confirm : bool)]);
    revert ();
    [%expect {|
      (called (confirm false)) |}];
    revert () ~confirm:true;
    [%expect {|
      (called (confirm true)) |}]);
;;

let%expect_test "[bury]" =
  print_s [%sexp (Buffer.all_live () : Buffer.t list)];
  [%expect {|
    ("#<buffer *scratch*>"
     "#<buffer  *Minibuf-0*>"
     "#<buffer *Messages*>"
     "#<buffer  *code-conversion-work*>"
     "#<buffer b1>"
     "#<buffer b2>"
     "#<buffer zzz>") |}];
  bury ();
  print_s [%sexp (Buffer.all_live () : Buffer.t list)];
  [%expect {|
    ("#<buffer  *Minibuf-0*>"
     "#<buffer *Messages*>"
     "#<buffer  *code-conversion-work*>"
     "#<buffer b1>"
     "#<buffer b2>"
     "#<buffer zzz>"
     "#<buffer *scratch*>") |}];
;;

let%expect_test "[paragraph_start], [paragraph_separate]" =
  let show x = print_s [%sexp (Current_buffer.value_exn x : Regexp.t)] in
  show paragraph_start;
  [%expect {|
    "\012\\|[ \t]*$" |}];
  show paragraph_separate;
  [%expect {|
    "[ \t\012]*$" |}];
;;
