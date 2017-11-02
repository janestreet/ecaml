open! Core_kernel
open! Import
open! Buffer

let show t = print_s [%sexp (t : t)]

let num_live () = List.length (all_live ())

let num_live_at_start = num_live ()

let%expect_test "[all_live]" =
  print_s [%sexp (all_live () : t list)];
  [%expect {|
    ("#<buffer *scratch*>"
     "#<buffer  *Minibuf-0*>"
     "#<buffer *Messages*>"
     "#<buffer  *code-conversion-work*>") |}];
;;

let require_clean () =
  require [%here] (num_live () = num_live_at_start);
;;

let%expect_test "[create]" =
  let t1 = create ~name:"foo" in
  show t1;
  [%expect {|
    "#<buffer foo>" |}];
  let t2 = create ~name:"foo" in
  show t2;
  [%expect {|
    "#<buffer foo<2>>" |}];
  kill t1;
  kill t2;
  require_clean ();
;;

let%expect_test "[kill]" =
  let t = find_or_create ~name:"test-buffer" in
  kill t;
  show t;
  [%expect {|
    "#<killed buffer>" |}];
  require_clean ();
;;

let%expect_test "[equal]" =
  let t1 = create ~name:"1" in
  let t2 = create ~name:"2" in
  require [%here] (equal t1 t1);
  require [%here] (not (equal t1 t2));
  kill t1;
  kill t2;
  require_clean ();
;;

let%expect_test "[name]" =
  let t = create ~name:"some-name" in
  print_s [%sexp (name t : string option)];
  [%expect {|
    (some-name) |}];
  kill t;
  print_s [%sexp (name t : string option)];
  [%expect {|
    () |}];
  require_clean ();
;;

let%expect_test "[file_name]" =
  let t = create ~name:"some-name" in
  print_s [%sexp (file_name t : string option)];
  [%expect {|
    () |}];
  kill t;
  print_s [%sexp (file_name t : string option)];
  [%expect {|
    () |}];
  require_clean ();
;;

let%expect_test "[is_live]" =
  let t = find_or_create ~name:"test-buffer" in
  let show () = print_s [%message "" ~is_live:(is_live t : bool)] in
  show ();
  [%expect {|
    (is_live true) |}];
  kill t;
  show ();
  [%expect {|
    (is_live false) |}];
  require_clean ();
;;

let%expect_test "[find] Some" =
  let t = create ~name:"foo" in
  print_s [%sexp (find ~name:"foo" : t option)];
  [%expect {|
    ("#<buffer foo>") |}];
  kill t;
  require_clean ();
;;

let%expect_test "[find] None" =
  print_s [%sexp (find ~name:"no buffer with this name" : t option)];
  [%expect {|
    () |}];
  require_clean ();
;;

let%expect_test "[find_or_create]" =
  let f () = find_or_create ~name:"test-buffer" in
  let t = f () in
  show t;
  [%expect {|
    "#<buffer test-buffer>" |}];
  require [%here] (equal t (f ()));
  kill t;
  require_clean ();
;;

let%expect_test "[displayed_in]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let t = Current_buffer.get () in
    let show_displayed_in () = print_s [%sexp (displayed_in t : Window.t list)] in
    show_displayed_in ();
    [%expect {|
      () |}];
    Selected_window.switch_to_buffer t;
    show_displayed_in ();
    [%expect {|
      ("#<window 1 on *temp-buffer*>") |}];
    Selected_window.split_vertically_exn ();
    show_displayed_in ();
    [%expect {|
      ("#<window 1 on *temp-buffer*>" "#<window 4 on *temp-buffer*>") |}]);
;;

let%expect_test "[display]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let t = Current_buffer.get () in
    let show_displayed_in () = print_s [%sexp (displayed_in t : Window.t list)] in
    show_displayed_in ();
    [%expect {|
      () |}];
    display t;
    show_displayed_in ();
    [%expect {|
      ("#<window 4 on *temp-buffer*>") |}]);
;;

let%expect_test "[buffer_local_value]" =
  let t = create ~name:"b" in
  let var = int_var "v" in
  Var.make_buffer_local_always var;
  Current_buffer.set_value var 13;
  Current_buffer.set_temporarily t ~f:(fun () ->
    Current_buffer.set_value var 14);
  print_s [%sexp (buffer_local_value (Current_buffer.get ()) var : int)];
  [%expect {|
    13 |}];
  print_s [%sexp (buffer_local_value t var : int)];
  [%expect {|
    14 |}];
;;

let%expect_test "[buffer_local_variables]" =
  let var = int_var "s" in
  Var.make_buffer_local_always var;
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    show_current_buffer_local_variables ();
    [%expect {|
      ((buffer-auto-save-file-format (_))
       (buffer-auto-save-file-name   (_))
       (buffer-backed-up             (_))
       (buffer-display-count         (_))
       (buffer-display-time          (_))
       (buffer-file-format           (_))
       (buffer-file-name             (_))
       (buffer-file-truename         (_))
       (buffer-invisibility-spec     (_))
       (buffer-read-only             (_))
       (buffer-saved-size            (_))
       (default-directory            (_))
       (enable-multibyte-characters  (_))
       (major-mode                   (_))
       (mark-active                  (_))
       (mode-name                    (_))
       (point-before-scroll          (_))) |}];
    Current_buffer.set_value var 14;
    show_current_buffer_local_variables ();
    [%expect {|
      ((buffer-auto-save-file-format (_))
       (buffer-auto-save-file-name   (_))
       (buffer-backed-up             (_))
       (buffer-display-count         (_))
       (buffer-display-time          (_))
       (buffer-file-format           (_))
       (buffer-file-name             (_))
       (buffer-file-truename         (_))
       (buffer-invisibility-spec     (_))
       (buffer-read-only             (_))
       (buffer-saved-size            (_))
       (default-directory            (_))
       (enable-multibyte-characters  (_))
       (major-mode                   (_))
       (mark-active                  (_))
       (mode-name                    (_))
       (point-before-scroll          (_))
       (s                            (_))) |}]);
;;

let%expect_test "[find_file_noselect] on a file that exists" =
  let t = find_file_noselect "test_buffer.ml" in
  show t;
  [%expect {|
    "#<buffer test_buffer.ml>" |}];
  kill t;
;;

let%expect_test "[find_file_noselect] on a non-existent file" =
  let t = find_file_noselect "zzz" in
  show t;
  [%expect {|
    "#<buffer zzz>" |}];
  kill t;
;;

let%expect_test "[save_some]" =
  let restore = unstage (ignore_stderr ()) in
  let save_some ?which_buffers () = save_some () ?which_buffers ~query:false in
  save_some ();
  let file = "z.tmp" in
  let is_modified () = print_s [%sexp (Current_buffer.is_modified () : bool)] in
  Selected_window.find_file file;
  Point.insert "foo";
  save_some ();
  is_modified ();
  [%expect {|
    false |}];
  Current_buffer.erase ();
  save_some () ~which_buffers:(These (fun b -> print_s [%sexp (b : Buffer.t)]; false));
  [%expect {|
    "#<buffer z.tmp>" |}];
  is_modified ();
  [%expect {|
    true |}];
  save_some () ~which_buffers:(These (fun b -> print_s [%sexp (b : Buffer.t)]; true));
  [%expect {|
    "#<buffer z.tmp>" |}];
  is_modified ();
  [%expect {|
    false |}];
  Current_buffer.kill ();
  File.delete file;
  restore ();
;;
