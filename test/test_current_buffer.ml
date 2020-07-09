open! Core_kernel
open! Async
open! Import
open! Current_buffer

let show () = print_s [%sexp (get () : Buffer.t)]
let show_point () = print_s [%message "" ~point:(Point.get () : Position.t)]

let%expect_test "[transient_mark_mode] is [true] in tests" =
  let show () = print_s [%sexp (Customization.value transient_mark_mode : bool)] in
  show ();
  [%expect {| true |}];
  return ()
;;

let%expect_test "[get]" =
  show ();
  [%expect {| "#<buffer *scratch*>" |}];
  return ()
;;

let%expect_test "[set]" =
  let old = get () in
  let t = Buffer.create ~name:"foo" in
  set t;
  show ();
  [%expect {| "#<buffer foo>" |}];
  set old;
  show ();
  [%expect {| "#<buffer *scratch*>" |}];
  Buffer.Blocking.kill t;
  return ()
;;

let%expect_test "[set_temporarily]" =
  let t = Buffer.create ~name:"foo" in
  set_temporarily Sync t ~f:(fun () ->
    set t;
    show ();
    [%expect {| "#<buffer foo>" |}]);
  show ();
  [%expect {| "#<buffer *scratch*>" |}];
  Buffer.Blocking.kill t;
  return ()
;;

let%expect_test "[set_temporarily] when [~f] raises" =
  let t = Buffer.create ~name:"foo" in
  show_raise ~hide_positions:true (fun () ->
    set_temporarily Sync t ~f:(fun () -> raise_s [%message [%here]]));
  [%expect {| (raised app/emacs/lib/ecaml/test/test_current_buffer.ml:LINE:COL) |}];
  show ();
  [%expect {| "#<buffer *scratch*>" |}];
  Buffer.Blocking.kill t;
  return ()
;;

let%expect_test "[set_temporarily_to_temp_buffer]" =
  set_temporarily_to_temp_buffer Sync (fun () -> show ());
  [%expect {| "#<buffer *temp-buffer*>" |}];
  show ();
  [%expect {| "#<buffer *scratch*>" |}];
  return ()
;;

let%expect_test "[value_exn] raise" =
  let var = int_var "z" in
  show_raise (fun () -> value_exn var);
  [%expect {| (raised ("[Current_buffer.value_exn] of undefined variable" (z int))) |}];
  return ()
;;

let%expect_test "[value] of unbound symbol" =
  print_s [%sexp (value (int_var "z") : int option)];
  [%expect {| () |}];
  return ()
;;

let%expect_test "[clear_value]" =
  let var = int_var "z" in
  let show () = print_s [%sexp (value var : int option)] in
  show ();
  [%expect {| () |}];
  set_value var 13;
  show ();
  [%expect {| (13) |}];
  clear_value var;
  show ();
  [%expect {| () |}];
  return ()
;;

let%expect_test "[set_value], [value_exn]" =
  let var = int_var "z" in
  set_value var 13;
  print_s [%sexp (value_exn var : int)];
  [%expect {| 13 |}];
  return ()
;;

let%expect_test "[has_non_null_value]" =
  let var = Var.create (Symbol.create ~name:"zzz") Value.Type.value in
  let show () = print_s [%sexp (has_non_null_value var : bool)] in
  show ();
  [%expect {| false |}];
  set_value var Value.nil;
  show ();
  [%expect {| false |}];
  set_value var Value.t;
  show ();
  [%expect {| true |}];
  set_value var (13 |> Value.of_int_exn);
  show ();
  [%expect {| true |}];
  return ()
;;

let%expect_test "[set_value_temporarily]" =
  let var = int_var "z" in
  let show () = print_s [%sexp (value_exn var : int)] in
  set_value var 13;
  show ();
  [%expect {| 13 |}];
  set_value_temporarily Sync var 14 ~f:(fun () ->
    show ();
    [%expect {| 14 |}]);
  show ();
  [%expect {| 13 |}];
  return ()
;;

let%expect_test "[set_value_temporarily] with an unbound var" =
  let var = int_var "z" in
  let show () = print_s [%sexp (value var : int option)] in
  show ();
  [%expect {| () |}];
  set_value_temporarily Sync var 13 ~f:(fun () ->
    show ();
    [%expect {| (13) |}]);
  show ();
  [%expect {| () |}];
  return ()
;;

let%expect_test "[set_value_temporarily] with a buffer-local and changed buffer" =
  let var = Var.create (Symbol.create ~name:"z") Value.Type.(nil_or int) in
  Var.make_buffer_local_always var;
  let b1 = Buffer.create ~name:"b1" in
  let b2 = Buffer.create ~name:"b2" in
  let value_in buffer = Buffer.buffer_local_value buffer var in
  let show_values () =
    print_s [%message "" ~b1:(value_in b1 : int option) ~b2:(value_in b2 : int option)]
  in
  show_values ();
  [%expect {|
    ((b1 ())
     (b2 ())) |}];
  Current_buffer.set_temporarily Sync b1 ~f:(fun () ->
    set_value var (Some 13);
    show_values ();
    [%expect {| ((b1 (13)) (b2 ())) |}];
    set_value_temporarily Sync var (Some 14) ~f:(fun () ->
      show_values ();
      [%expect {| ((b1 (14)) (b2 ())) |}];
      Current_buffer.set b2);
    show_values ();
    [%expect {| ((b1 (13)) (b2 ())) |}]);
  return ()
;;

let%expect_test "[set_values_temporarily]" =
  let v1 = int_var "z1" in
  let v2 = int_var "z2" in
  let show () = print_s [%message "" ~_:(value_exn v1 : int) ~_:(value_exn v2 : int)] in
  set_value v1 13;
  set_value v2 14;
  show ();
  [%expect {| (13 14) |}];
  set_values_temporarily
    Sync
    [ T (v1, 15); T (v2, 16) ]
    ~f:(fun () ->
      show ();
      [%expect {| (15 16) |}]);
  show ();
  [%expect {| (13 14) |}];
  return ()
;;

let%expect_test "[file_name]" =
  let show_file_basename () =
    print_s [%sexp (file_name () |> Option.map ~f:Filename.nondirectory : string option)]
  in
  show_file_basename ();
  [%expect {| () |}];
  let%bind () = Selected_window.find_file "test_current_buffer.ml" in
  show_file_basename ();
  [%expect {| (test_current_buffer.ml) |}];
  Buffer.Blocking.kill (get ());
  return ()
;;

let%expect_test "[file_name_exn] raise" =
  set_temporarily_to_temp_buffer Sync (fun () -> show_raise (fun () -> file_name_exn ()));
  [%expect {| (raised ("buffer does not have a file name" "#<buffer *temp-buffer*>")) |}];
  return ()
;;

let%expect_test "[is_modified], [set_modified]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    print_s [%sexp (is_modified () : bool)];
    [%expect {| false |}];
    Point.insert "foo";
    print_s [%sexp (is_modified () : bool)];
    [%expect {| true |}];
    set_modified false;
    print_s [%sexp (is_modified () : bool)];
    [%expect {| false |}]);
  return ()
;;

let%expect_test "[contents]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    print_s [%sexp (contents () : Text.t)];
    [%expect {| "" |}];
    Point.insert "foo";
    print_s [%sexp (contents () : Text.t)];
    [%expect {| foo |}];
    print_s
      [%sexp
        (contents () ~start:(1 |> Position.of_int_exn) ~end_:(2 |> Position.of_int_exn)
         : Text.t)];
    [%expect {| f |}]);
  return ()
;;

let%expect_test "[contents_text ~text_properties:true], [insert_text]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    let show () = print_s [%sexp (contents ~text_properties:true () : Text.t)] in
    show ();
    [%expect {| "" |}];
    Point.insert_text
      (Text.propertize
         (Text.of_utf8_bytes "foo")
         [ T (Text.Property_name.face, background_red) ]);
    show ();
    [%expect {| (foo 0 3 (face (:background red))) |}];
    print_s
      [%sexp
        (contents
           ()
           ~start:(1 |> Position.of_int_exn)
           ~end_:(2 |> Position.of_int_exn)
           ~text_properties:true
         : Text.t)];
    [%expect {| (f 0 1 (face (:background red))) |}]);
  return ()
;;

let%expect_test "[erase]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    erase ();
    Point.insert "foo";
    erase ();
    print_s [%sexp (contents () : Text.t)];
    [%expect {| "" |}]);
  return ()
;;

let%expect_test "[kill_region]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "foobar";
    kill_region ~start:(2 |> Position.of_int_exn) ~end_:(4 |> Position.of_int_exn);
    print_s [%sexp (contents () : Text.t)];
    [%expect {| fbar |}]);
  return ()
;;

let%expect_test "[save_excursion]" =
  let i = save_excursion Sync (fun () -> 13) in
  print_s [%sexp (i : int)];
  [%expect {| 13 |}];
  return ()
;;

let%expect_test "[save_excursion] preserves point" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    show_point ();
    save_excursion Sync (fun () ->
      Point.insert "foo";
      show_point ());
    show_point ());
  [%expect {|
    (point 1)
    (point 4)
    (point 1) |}];
  return ()
;;

let%expect_test "[save_excursion] preserves current buffer" =
  let t = Buffer.find_or_create ~name:"zzz" in
  save_excursion Sync (fun () ->
    set t;
    show ());
  show ();
  [%expect {|
    "#<buffer zzz>"
    "#<buffer *scratch*>" |}];
  return ()
;;

let%expect_test "[set_multibyte], [is_multibyte]" =
  let show () = print_s [%message "" ~is_multibyte:(is_multibyte () : bool)] in
  show ();
  [%expect {| (is_multibyte true) |}];
  set_multibyte false;
  show ();
  [%expect {| (is_multibyte false) |}];
  set_multibyte true;
  show ();
  [%expect {| (is_multibyte true) |}];
  return ()
;;

let%expect_test "[rename_exn]" =
  let t = Buffer.create ~name:"foo" in
  set_temporarily Sync t ~f:(fun () ->
    rename_exn () ~name:"bar";
    show ());
  [%expect {| "#<buffer bar>" |}];
  Buffer.Blocking.kill t;
  return ()
;;

let%expect_test "[rename_exn] raise" =
  let t1 = Buffer.create ~name:"foo" in
  let t2 = Buffer.create ~name:"bar" in
  require_does_raise [%here] (fun () ->
    set_temporarily Sync t1 ~f:(fun () ->
      rename_exn () ~name:(Buffer.name t2 |> Option.value_exn)));
  [%expect {| ("Buffer name `bar' is in use") |}];
  Buffer.Blocking.kill t1;
  Buffer.Blocking.kill t2;
  return ()
;;

let%expect_test "[rename_exn ~unique:true]" =
  let t1 = Buffer.create ~name:"foo" in
  set_temporarily Sync t1 ~f:(fun () ->
    rename_exn () ~name:"bar" ~unique:true;
    show ());
  [%expect {| "#<buffer bar>" |}];
  let t2 = Buffer.create ~name:"foo" in
  set_temporarily Sync t2 ~f:(fun () ->
    rename_exn () ~name:"bar" ~unique:true;
    show ());
  [%expect {| "#<buffer bar<2>>" |}];
  Buffer.Blocking.kill t1;
  Buffer.Blocking.kill t2;
  return ()
;;

let%expect_test "[text_property_is_present]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    let show () =
      print_s [%sexp (text_property_is_present Text.Property_name.face : bool)]
    in
    show ();
    [%expect {| false |}];
    let insert property_name =
      Point.insert_text
        (Text.propertize
           (Text.of_utf8_bytes "foo")
           [ T (property_name, background_red) ])
    in
    insert Text.Property_name.font_lock_face;
    show ();
    [%expect {| false |}];
    insert Text.Property_name.face;
    show ();
    [%expect {| true |}]);
  return ()
;;

let%expect_test "[is_read_only], [set_read_only]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    let show () =
      print_s [%message "" ~is_read_only:(get_buffer_local read_only : bool)]
    in
    show ();
    [%expect {| (is_read_only false) |}];
    set_buffer_local read_only true;
    show ();
    [%expect {| (is_read_only true) |}];
    set_buffer_local read_only false;
    show ();
    [%expect {| (is_read_only false) |}]);
  return ()
;;

let face = Text.Property_name.face
let font_lock_face = Text.Property_name.font_lock_face
let show () = print_s [%sexp (contents ~text_properties:true () : Text.t)]

let%expect_test "[set_text_property]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "foo";
    set_text_property face foreground_red;
    show ();
    [%expect {| (foo 0 3 (face (:foreground red))) |}];
    set_text_property face foreground_blue;
    show ();
    [%expect {| (foo 0 3 (face (:foreground blue))) |}]);
  return ()
;;

let%expect_test "[set_text_property_staged]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "foo";
    let set = unstage (set_text_property_staged face foreground_red) in
    set ~start:1 ~end_:2;
    set ~start:2 ~end_:3;
    show ();
    [%expect {| (foo 0 1 (face (:foreground red)) 1 2 (face (:foreground red))) |}]);
  return ()
;;

let%expect_test "[set_text_properties]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "foo";
    set_text_properties [ T (face, foreground_red) ];
    show ();
    [%expect {| (foo 0 3 (face (:foreground red))) |}];
    set_text_properties [ T (font_lock_face, foreground_red) ];
    show ();
    [%expect {| (foo 0 3 (font-lock-face (:foreground red))) |}]);
  return ()
;;

let%expect_test "[set_text_properties_staged]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    let show () = print_s [%sexp (contents ~text_properties:true () : Text.t)] in
    Point.insert "foo";
    let set = unstage (set_text_properties_staged [ T (face, foreground_red) ]) in
    set ~start:1 ~end_:2;
    set ~start:2 ~end_:3;
    show ();
    [%expect {| (foo 0 1 (face (:foreground red)) 1 2 (face (:foreground red))) |}];
    set_text_property face foreground_blue;
    set ~start:2 ~end_:3;
    show ();
    [%expect
      {|
      (foo 0 1
        (face (:foreground blue))
        1
        2
        (face (:foreground red))
        2
        3
        (face (:foreground blue))) |}]);
  return ()
;;

let%expect_test "[add_text_properties]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "foo";
    add_text_properties [ T (face, foreground_red) ];
    show ();
    [%expect {| (foo 0 3 (face (:foreground red))) |}];
    add_text_properties [ T (font_lock_face, foreground_red) ];
    show ();
    [%expect {| (foo 0 3 (face (:foreground red) font-lock-face (:foreground red))) |}]);
  return ()
;;

let%expect_test "[add_text_properties_staged]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "foo";
    set_text_property font_lock_face foreground_blue;
    let add = unstage (add_text_properties_staged [ T (face, foreground_red) ]) in
    add ~start:1 ~end_:2;
    add ~start:2 ~end_:3;
    show ();
    [%expect
      {|
      (foo 0 1
        (font-lock-face (:foreground blue) face (:foreground red))
        1
        2
        (font-lock-face (:foreground blue) face (:foreground red))
        2
        3
        (font-lock-face (:foreground blue))) |}]);
  return ()
;;

let%expect_test "[is_undo_enabled] [set_undo_enabled]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    let show () = print_s [%message "" ~is_undo_enabled:(is_undo_enabled () : bool)] in
    show ();
    [%expect {| (is_undo_enabled true) |}];
    set_undo_enabled false;
    show ();
    [%expect {| (is_undo_enabled false) |}];
    set_undo_enabled true;
    show ();
    [%expect {| (is_undo_enabled true) |}]);
  return ()
;;

let%expect_test "[undo], [add_undo_boundary]" =
  let restore_stderr = unstage (ignore_stderr ()) in
  set_temporarily_to_temp_buffer Sync (fun () ->
    let show () =
      print_s
        [%message
          "" ~contents:(contents () : Text.t) ~undo_list:(undo_list () : Value.t)]
    in
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
    [%expect
      {|
      ((contents foo)
       (undo_list (
         (1 . 4)
         (t . 0)))) |}];
    add_undo_boundary ();
    show ();
    [%expect
      {|
      ((contents foo)
       (undo_list (
         nil
         (1 . 4)
         (t . 0)))) |}];
    undo 1;
    show ();
    [%expect
      {|
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
    [%expect
      {|
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
    [%expect
      {|
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
  return ()
;;

let%expect_test "[mark], [set_mark]" =
  let show () = print_s [%sexp (mark () : Marker.t)] in
  set_temporarily_to_temp_buffer Sync (fun () ->
    show ();
    [%expect {| "#<marker in no buffer>" |}];
    Point.insert "foo";
    set_mark (2 |> Position.of_int_exn);
    show ();
    [%expect {| "#<marker at 2 in *temp-buffer*>" |}]);
  return ()
;;

let%expect_test "[mark_is_active], [deactivate_mark]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    let show () = print_s [%sexp (mark_is_active () : bool)] in
    show ();
    [%expect {| false |}];
    set_mark (1 |> Position.of_int_exn);
    require [%here] (mark_is_active ());
    show ();
    [%expect {| true |}];
    deactivate_mark ();
    require [%here] (not (mark_is_active ()));
    show ();
    [%expect {| false |}]);
  return ()
;;

let%expect_test "[set_marker_position]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    let marker = Marker.create () in
    let set i =
      set_marker_position marker (i |> Position.of_int_exn);
      print_s [%sexp (marker : Marker.t)]
    in
    set 1;
    [%expect {| "#<marker at 1 in *temp-buffer*>" |}];
    set 2;
    [%expect {| "#<marker at 1 in *temp-buffer*>" |}];
    Point.insert "foo";
    set 2;
    [%expect {| "#<marker at 2 in *temp-buffer*>" |}]);
  return ()
;;

let show_var var =
  print_s
    [%message
      ""
        ~value:
          (Current_buffer.value { var with type_ = Value.Type.value } : Value.t option)
        ~is_buffer_local:(is_buffer_local var : bool)
        ~is_buffer_local_if_set:(is_buffer_local_if_set var : bool)]
;;

let%expect_test "[make_buffer_local], [is_buffer_local], [is_buffer_local_if_set]" =
  let var = int_var "s" in
  set_temporarily_to_temp_buffer Sync (fun () ->
    make_buffer_local var;
    Current_buffer.set_value var 13;
    show_var var;
    [%expect
      {|
      ((value (13))
       (is_buffer_local        true)
       (is_buffer_local_if_set true)) |}]);
  show_var var;
  [%expect
    {|
    ((value ())
     (is_buffer_local        false)
     (is_buffer_local_if_set false)) |}];
  return ()
;;

let%expect_test "[kill_buffer_local]" =
  let var = int_var "z" in
  Current_buffer.set_value var 13;
  set_temporarily_to_temp_buffer Sync (fun () ->
    make_buffer_local var;
    Current_buffer.set_value var 14;
    show_var var;
    [%expect
      {|
      ((value (14))
       (is_buffer_local        true)
       (is_buffer_local_if_set true)) |}];
    kill_buffer_local var;
    show_var var;
    [%expect
      {|
      ((value (13))
       (is_buffer_local        false)
       (is_buffer_local_if_set false)) |}]);
  return ()
;;

let%expect_test "[local_keymap], [set_local_keymap]" =
  let show_local_keymap () = print_s [%sexp (local_keymap () : Keymap.t option)] in
  set_temporarily_to_temp_buffer Sync (fun () ->
    show_local_keymap ();
    [%expect {| () |}];
    let keymap = Keymap.create () in
    Keymap.define_key keymap ("a" |> Key_sequence.create_exn) Undefined;
    set_local_keymap keymap;
    show_local_keymap ();
    [%expect {| ((keymap (97 . undefined))) |}]);
  return ()
;;

let%expect_test "[minor_mode_keymaps]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Minor_mode.(enable view);
    print_s [%sexp (minor_mode_keymaps () : Keymap.t list)];
    [%expect
      {|
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
  return ()
;;

let%expect_test "[delete_duplicate_lines]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "a\na\nb\nb\nc\na\n";
    delete_duplicate_lines ();
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      a
      b
      c |}]);
  return ()
;;

let%expect_test "[delete_lines_matching]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "a\na\nb\nb\nc\na\n";
    delete_lines_matching ("b" |> Regexp.quote);
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      a
      a
      c
      a |}]);
  return ()
;;

let%expect_test "[sort_lines]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "a\nd\nb\nc\n";
    sort_lines ();
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      a
      b
      c
      d |}]);
  return ()
;;

let%expect_test "[delete_region]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert "123456789";
    delete_region ~start:(3 |> Position.of_int_exn) ~end_:(5 |> Position.of_int_exn);
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {| 1256789 |}]);
  return ()
;;

let%expect_test "[indent_region]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Funcall.Wrap.("c-mode" <: nullary @-> return nil) ();
    Point.insert "void f () {\n      foo;\n      bar;\n      }";
    indent_region ();
    print_string (contents () |> Text.to_utf8_bytes);
    [%expect {|
      void f () {
        foo;
        bar;
      } |}]);
  return ()
;;

let%expect_test "[set_revert_buffer_function (Returns_deferred Value.Type.unit)]" =
  set_temporarily_to_temp_buffer Async (fun () ->
    set_revert_buffer_function
      [%here]
      (Returns_deferred Value.Type.unit)
      (fun ~confirm ->
         let%map () = Clock.after (sec 0.01) in
         print_s [%message "called after pause" (confirm : bool)]);
    let%bind () = revert () in
    [%expect {| ("called after pause" (confirm false)) |}];
    let%bind () = revert () ~confirm:true in
    [%expect {| ("called after pause" (confirm true)) |}];
    return ())
;;

let%expect_test "[set_values_temporarily async]" =
  let v1 = int_var "z1" in
  let v2 = int_var "z2" in
  let show () = print_s [%message "" ~_:(value_exn v1 : int) ~_:(value_exn v2 : int)] in
  set_value v1 13;
  set_value v2 14;
  show ();
  [%expect {|
    (13 14) |}];
  (* Incorrectly using [set_values_temporarily _ _ Sync] on an Async value, we get the
     wrong result *)
  let%bind () =
    set_values_temporarily
      Sync
      [ T (v1, 15); T (v2, 16) ]
      ~f:(fun () ->
        let%bind () = Clock.after (Time.Span.of_ms 1.) in
        show ();
        [%expect {| (13 14) |}];
        return ())
  in
  (* Correctly using [set_values_temporarily _ _ Async], we get a correct result. *)
  let%bind () =
    set_values_temporarily
      Async
      [ T (v1, 15); T (v2, 16) ]
      ~f:(fun () ->
        let%bind () = Clock.after (Time.Span.of_ms 1.) in
        show ();
        [%expect {| (15 16) |}];
        return ())
  in
  show ();
  [%expect {| (13 14) |}];
  return ()
;;

let%expect_test "[set_value_temporarily async]" =
  let var = int_var "z" in
  let show () = print_s [%sexp (value_exn var : int)] in
  set_value var 13;
  show ();
  [%expect {|
    13 |}];
  (* Incorrectly using [set_value_temporarily _ _ Sync] on an Async value, we get the
     wrong result *)
  let%bind () =
    set_value_temporarily Sync var 14 ~f:(fun () ->
      let%bind () = Clock.after (Time.Span.of_ms 1.) in
      show ();
      [%expect {| 13 |}];
      return ())
  in
  (* Correctly using [set_value_temporarily _ _ Async], we get a correct result. *)
  let%bind () =
    set_value_temporarily Async var 14 ~f:(fun () ->
      let%bind () = Clock.after (Time.Span.of_ms 1.) in
      show ();
      [%expect {| 14 |}];
      return ())
  in
  show ();
  [%expect {| 13 |}];
  return ()
;;

let show () = print_s [%sexp (get () : Buffer.t)]

let%expect_test "[set_temporarily async]" =
  let t = Buffer.create ~name:"foo" in
  let%bind () =
    set_temporarily Async t ~f:(fun () ->
      let%bind () = Clock.after (sec 0.001) in
      set t;
      show ();
      [%expect {| "#<buffer foo>" |}];
      return ())
  in
  show ();
  [%expect {|
    "#<buffer *scratch*>" |}];
  Buffer.kill t
;;

let%expect_test "[set_temporarily_to_temp_buffer async]" =
  let%bind () =
    set_temporarily_to_temp_buffer Async (fun () ->
      let%map () = Clock.after (sec 0.001) in
      show ())
  in
  [%expect {| "#<buffer *temp-buffer*>" |}];
  show ();
  [%expect {| "#<buffer *scratch*>" |}];
  return ()
;;

let%expect_test "[set_temporarily_to_temp_buffer Async] from background job" =
  let%bind () =
    Deferred.create (fun background_job_done ->
      Background.don't_wait_for [%here] (fun () ->
        let%bind () =
          show_raise_async ~hide_positions:true (fun () ->
            set_temporarily_to_temp_buffer Async return)
        in
        Ivar.fill background_job_done ();
        return ()))
  in
  [%expect
    {|
      (raised (
        "Assertion failed -- running in background job"
        ((background_job_started_at
          app/emacs/lib/ecaml/test/test_current_buffer.ml:LINE:COL)
         (assertion_failed_at app/emacs/lib/ecaml/src/current_buffer.ml:LINE:COL)))) |}];
  return ()
;;

let%expect_test "[save_excursion] Async" =
  set_temporarily_to_temp_buffer Async (fun () ->
    Point.insert "some random text so we have space to work with";
    let show_point () = print_s [%sexp (Point.get () : Position.t)] in
    show_point ();
    [%expect {| 47 |}];
    let%bind () =
      save_excursion Async (fun () ->
        Point.goto_char (Position.of_int_exn 5);
        let%bind () = Clock.after (sec 0.001) in
        show_point ();
        [%expect {| 5 |}];
        return ())
    in
    show_point ();
    [%expect {| 47 |}];
    return ())
;;

let%expect_test "[save_excursion Async] from background job" =
  let%bind () =
    Deferred.create (fun background_job_done ->
      Background.don't_wait_for [%here] (fun () ->
        let%bind () =
          show_raise_async ~hide_positions:true (fun () ->
            save_excursion Async return)
        in
        Ivar.fill background_job_done ();
        return ()))
  in
  [%expect
    {|
      (raised (
        "Assertion failed -- running in background job"
        ((background_job_started_at
          app/emacs/lib/ecaml/test/test_current_buffer.ml:LINE:COL)
         (assertion_failed_at app/emacs/lib/ecaml/src/save_wrappers.ml:LINE:COL))
        "save-excursion called asynchronously in background job")) |}];
  return ()
;;

let%expect_test "[set_temporarily] killing old buffer" =
  let buffer = Buffer.create ~name:"z" in
  let old = get () in
  let%bind () =
    set_temporarily Async buffer ~f:(fun () ->
      let%bind () = Buffer.kill old in
      show ();
      [%expect {| "#<buffer z>" |}];
      return ())
  in
  show ();
  [%expect {| "#<buffer z>" |}];
  print_s [%sexp (Buffer.is_live (get ()) : bool)];
  [%expect {| true |}];
  print_s [%sexp (Buffer.is_live old : bool)];
  [%expect {| false |}];
  return ()
;;

let%expect_test "[set_revert_buffer_function]" =
  set_temporarily_to_temp_buffer Async (fun () ->
    set_revert_buffer_function [%here] (Returns Value.Type.unit) (fun ~confirm ->
      print_s [%message "called" (confirm : bool)]);
    let%bind () = revert () in
    [%expect {| (called (confirm false)) |}];
    let%bind () = revert () ~confirm:true in
    [%expect {| (called (confirm true)) |}];
    return ())
;;

let%expect_test "[kill]" =
  let buffer = Buffer.create ~name:"z" in
  let show_live () = print_s [%sexp (Buffer.is_live buffer : bool)] in
  show_live ();
  [%expect {| true |}];
  set buffer;
  let%bind () = kill () in
  show_live ();
  [%expect {| false |}];
  return ()
;;

let%expect_test "[kill] with deferred kill hook" =
  let buffer = Buffer.create ~name:"z" in
  set buffer;
  Ecaml.Hook.add
    ~buffer_local:true
    Ecaml.Hook.kill_buffer
    (Ecaml.Hook.Function.create
       ("test-deferred-kill-hook" |> Symbol.intern)
       [%here]
       ~hook_type:Normal
       (Returns_deferred Value.Type.unit)
       (fun () ->
          let%bind () = Clock.after (sec 0.001) in
          print_s [%sexp "deferred hook ran"];
          return ()));
  let%bind () = kill () in
  [%expect {| "deferred hook ran" |}];
  return ()
;;

let%expect_test "[save]" =
  let file = Caml.Filename.temp_file "" "" in
  let%bind () = Selected_window.find_file file in
  Point.insert "foobar";
  let%bind () = save () in
  let%bind () = kill () in
  let%bind () =
    set_temporarily_to_temp_buffer Async (fun () ->
      Point.insert_file_contents_exn file;
      print_s [%sexp (contents () : Text.t)];
      [%expect {| foobar |}];
      return ())
  in
  Sys.remove file
;;

let%expect_test "[bury]" =
  print_s [%sexp (Buffer.all_live () : Buffer.t list)];
  [%expect
    {|
    ("#<buffer  *Minibuf-0*>"
     "#<buffer *Messages*>"
     "#<buffer  *code-conversion-work*>"
     "#<buffer b1>"
     "#<buffer b2>"
     "#<buffer zzz>"
     "#<buffer z>") |}];
  bury ();
  print_s [%sexp (Buffer.all_live () : Buffer.t list)];
  [%expect
    {|
    ("#<buffer  *Minibuf-0*>"
     "#<buffer  *code-conversion-work*>"
     "#<buffer b1>"
     "#<buffer b2>"
     "#<buffer zzz>"
     "#<buffer z>"
     "#<buffer *Messages*>") |}];
  return ()
;;

let%expect_test "[paragraph_start], [paragraph_separate]" =
  let show x = print_s [%sexp (Current_buffer.value_exn x : Regexp.t)] in
  show paragraph_start;
  [%expect {| "\012\\|[ \t]*$" |}];
  show paragraph_separate;
  [%expect {| "[ \t\012]*$" |}];
  return ()
;;

let%expect_test "[describe_mode]" =
  let%bind () =
    Current_buffer.set_temporarily_to_temp_buffer Async (fun () ->
      let%bind () =
        Current_buffer.change_major_mode Major_mode.Fundamental.major_mode
      in
      describe_mode ();
      Selected_window.other_window 1;
      print_endline
        (Current_buffer.contents ()
         |> Text.to_utf8_bytes
         |> String.tr ~target:'\012' ~replacement:'\n');
      Selected_window.quit ())
  in
  [%expect
    {|
    Type C-x 1 to delete the help window, C-M-v to scroll help.
    Enabled minor modes: Auto-Composition Auto-Compression Auto-Encryption
    Electric-Indent File-Name-Shadow Global-Eldoc Line-Number Mouse-Wheel
    Tooltip Transient-Mark

    (Information about these minor modes follows the major mode info.)

     mode defined in `simple.el':
    Major mode not specialized for anything in particular.
    Other major modes are defined by comparison with this one.


    Auto-Composition minor mode (no indicator):
    Toggle Auto Composition mode.
    With a prefix argument ARG, enable Auto Composition mode if ARG
    is positive, and disable it otherwise.  If called from Lisp,
    enable the mode if ARG is omitted or nil.

    When Auto Composition mode is enabled, text characters are
    automatically composed by functions registered in
    `composition-function-table'.

    You can use `global-auto-composition-mode' to turn on
    Auto Composition mode in all buffers (this is the default).


    Auto-Compression minor mode (no indicator):
    Toggle Auto Compression mode.
    With a prefix argument ARG, enable Auto Compression mode if ARG
    is positive, and disable it otherwise.  If called from Lisp,
    enable the mode if ARG is omitted or nil.

    Auto Compression mode is a global minor mode.  When enabled,
    compressed files are automatically uncompressed for reading, and
    compressed when writing.


    Auto-Encryption minor mode (no indicator):
    Toggle automatic file encryption/decryption (Auto Encryption mode).
    With a prefix argument ARG, enable Auto Encryption mode if ARG is
    positive, and disable it otherwise.  If called from Lisp, enable
    the mode if ARG is omitted or nil.

    (fn &optional ARG)


    Electric-Indent minor mode (no indicator):
    Toggle on-the-fly reindentation (Electric Indent mode).
    With a prefix argument ARG, enable Electric Indent mode if ARG is
    positive, and disable it otherwise.  If called from Lisp, enable
    the mode if ARG is omitted or nil.

    When enabled, this reindents whenever the hook `electric-indent-functions'
    returns non-nil, or if you insert a character from `electric-indent-chars'.

    This is a global minor mode.  To toggle the mode in a single buffer,
    use `electric-indent-local-mode'.


    File-Name-Shadow minor mode (no indicator):
    Toggle file-name shadowing in minibuffers (File-Name Shadow mode).
    With a prefix argument ARG, enable File-Name Shadow mode if ARG
    is positive, and disable it otherwise.  If called from Lisp,
    enable the mode if ARG is omitted or nil.

    File-Name Shadow mode is a global minor mode.  When enabled, any
    part of a filename being read in the minibuffer that would be
    ignored (because the result is passed through
    `substitute-in-file-name') is given the properties in
    `file-name-shadow-properties', which can be used to make that
    portion dim, invisible, or otherwise less visually noticeable.


    Global-Eldoc minor mode (no indicator):
    Toggle Eldoc mode in all buffers.
    With prefix ARG, enable Global Eldoc mode if ARG is positive;
    otherwise, disable it.  If called from Lisp, enable the mode if
    ARG is omitted or nil.

    Eldoc mode is enabled in all buffers where
    `turn-on-eldoc-mode' would do it.
    See `eldoc-mode' for more information on Eldoc mode.

    (fn &optional ARG)


    Line-Number minor mode (no indicator):
    Toggle line number display in the mode line (Line Number mode).
    With a prefix argument ARG, enable Line Number mode if ARG is
    positive, and disable it otherwise.  If called from Lisp, enable
    the mode if ARG is omitted or nil.

    Line numbers do not appear for very large buffers and buffers
    with very long lines; see variables `line-number-display-limit'
    and `line-number-display-limit-width'.

    (fn &optional ARG)


    Mouse-Wheel minor mode (no indicator):
    Toggle mouse wheel support (Mouse Wheel mode).
    With a prefix argument ARG, enable Mouse Wheel mode if ARG is
    positive, and disable it otherwise.  If called from Lisp, enable
    the mode if ARG is omitted or nil.


    Tooltip minor mode (no indicator):
    Toggle Tooltip mode.
    With a prefix argument ARG, enable Tooltip mode if ARG is positive,
    and disable it otherwise.  If called from Lisp, enable the mode
    if ARG is omitted or nil.

    When this global minor mode is enabled, Emacs displays help
    text (e.g. for buttons and menu items that you put the mouse on)
    in a pop-up window.

    When Tooltip mode is disabled, Emacs displays help text in the
    echo area, instead of making a pop-up window.


    Transient-Mark minor mode (no indicator):
    Toggle Transient Mark mode.
    With a prefix argument ARG, enable Transient Mark mode if ARG is
    positive, and disable it otherwise.  If called from Lisp, enable
    Transient Mark mode if ARG is omitted or nil.

    Transient Mark mode is a global minor mode.  When enabled, the
    region is highlighted with the `region' face whenever the mark
    is active.  The mark is "deactivated" by changing the buffer,
    and after certain other operations that set the mark but whose
    main purpose is something else--for example, incremental search,
    <, and >.

    You can also deactivate the mark by typing C-g or
    M-ESC ESC.

    Many commands change their behavior when Transient Mark mode is
    in effect and the mark is active, by acting on the region instead
    of their usual default part of the buffer's text.  Examples of
    such commands include M-;, M-x flush-lines, M-x keep-lines,
    M-%, C-M-%, M-x ispell, and C-x u.
    To see the documentation of commands which are sensitive to the
    Transient Mark mode, invoke C-h d and type "transient"
    or "mark.*active" at the prompt.

    (fn &optional ARG) |}];
  return ()
;;

let%expect_test "[chars_modified_tick]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    print_s [%sexp (chars_modified_tick () : Modified_tick.t)];
    Point.insert "foo";
    print_s [%sexp (chars_modified_tick () : Modified_tick.t)]);
  [%expect {|
    1
    2 |}];
  return ()
;;

let%expect_test "[replace_string]" =
  set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert {|
abc
def
abc
def
|};
    replace_string ~from:"ab" ~to_:"ba" ();
    print_endline (contents () |> Text.to_utf8_bytes);
    [%expect {|
      bac
      def
      bac
      def |}]);
  return ()
;;
