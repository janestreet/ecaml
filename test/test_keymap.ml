open! Core_kernel
open! Import
open! Keymap

let show t = print_s [%sexp (t : t)]

let define_key t key entry = define_key t (Key_sequence.create_exn key) entry

let lookup_key_exn t key = lookup_key_exn t (Key_sequence.create_exn key)

let show_keys t keys =
  List.iter keys ~f:(fun key ->
    print_s [%message
      ""
        ~_:(key : string)
        ~_:(lookup_key_exn t key : Entry.t)]);
;;

let%expect_test "[global], [lookup_key_exn]" =
  show_keys (global ())
    [ "a"
    ; "C-c"
    ; "C-x C-a"
    ; "C-x C-f"
    ; "ESC" ];
  [%expect {|
    (a (Command self-insert-command))
    (C-c (Keymap mode-specific-command-prefix))
    ("C-x C-a" Absent)
    ("C-x C-f" (Command find-file))
    (ESC (Keymap ESC-prefix)) |}];
;;

let%expect_test "[create]" =
  let t = create () in
  show t;
  [%expect {|
    (keymap) |}];
;;

let%expect_test "[define_key]" =
  let t = create () in
  define_key t "a" (Symbol ("zzz" |> Symbol.intern));
  show t;
  [%expect {|
    (keymap (97 . zzz)) |}];
  define_key t "bc" (Symbol ("yyy" |> Symbol.intern));
  show t;
  [%expect {|
    (keymap (98 keymap (99 . yyy)) (97 . zzz)) |}];
  define_key t "bd" Undefined;
  show t;
  [%expect {|
    (keymap
      (98 keymap
        (100 . undefined)
        (99  . yyy))
      (97 . zzz)) |}];
  define_key t "bd" Absent;
  show t;
  [%expect {|
    (keymap (98 keymap (100) (99 . yyy)) (97 . zzz)) |}];
  define_key t "b" Absent;
  show t;
  [%expect {|
    (keymap (98) (97 . zzz)) |}];
;;

let%expect_test "[lookup_key_exn] too long" =
  show_raise (fun () -> lookup_key_exn (create ()) "ab");
  [%expect {|
    (raised (
      "[Keymap.lookup_key_exn] got too long key sequence" (key_sequence "a b"))) |}];
;;

let%expect_test "[set_global]" =
  let saved = global () in
  let t = create () in
  set_global t;
  require [%here] (eq t (global ()));
  set_global saved;
;;

let%expect_test "[parent], [set_parent]" =
  let t = create () in
  print_s [%sexp (parent t : t option)];
  [%expect {|
    () |}];
  set_parent t (Some (create ()));
  print_s [%sexp (parent t : t option)];
  [%expect {|
    ((keymap)) |}];
  set_parent t None;
  print_s [%sexp (parent t : t option)];
  [%expect {|
    () |}];
;;

let%expect_test "[deep_copy]" =
  let t1 = create () in
  define_key t1 "abc" Undefined;
  let t2 = deep_copy t1 in
  define_key t2 "abc" Absent;
  show t1;
  [%expect {|
    (keymap (97 keymap (98 keymap (99 . undefined)))) |}];
  show t2;
  [%expect {|
    (keymap (97 keymap (98 keymap (99)))) |}];
;;

let%expect_test "[create ~kind:Full ()]" =
  let t = create ~kind:Full () in
  show t;
  [%expect {|
    (keymap
     #^[nil
     nil
     keymap
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil
     nil]) |}];
;;
