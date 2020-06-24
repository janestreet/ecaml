open! Core_kernel
open! Async_kernel
open! Import
open! Buffer

let show t = print_s [%sexp (t : t)]
let num_live () = List.length (all_live ())
let num_live_at_start = num_live ()

let%expect_test "[all_live]" =
  print_s [%sexp (all_live () : t list)];
  [%expect
    {|
    ("#<buffer *scratch*>"
     "#<buffer  *Minibuf-0*>"
     "#<buffer *Messages*>"
     "#<buffer  *code-conversion-work*>") |}];
  return ()
;;

let require_clean () = require [%here] (num_live () = num_live_at_start)

let%expect_test "[create]" =
  let t1 = create ~name:"foo" in
  show t1;
  [%expect {|
    "#<buffer foo>" |}];
  let t2 = create ~name:"foo" in
  show t2;
  [%expect {|
    "#<buffer foo<2>>" |}];
  Blocking.kill t1;
  Blocking.kill t2;
  require_clean ();
  return ()
;;

let%expect_test "[Blocking.kill]" =
  let t = find_or_create ~name:"test-buffer" in
  Blocking.kill t;
  show t;
  [%expect {| "#<killed buffer>" |}];
  require_clean ();
  return ()
;;

let%expect_test "[equal]" =
  let t1 = create ~name:"1" in
  let t2 = create ~name:"2" in
  require [%here] (equal t1 t1);
  require [%here] (not (equal t1 t2));
  Blocking.kill t1;
  Blocking.kill t2;
  require_clean ();
  return ()
;;

let%expect_test "[name]" =
  let t = create ~name:"some-name" in
  print_s [%sexp (name t : string option)];
  [%expect {| (some-name) |}];
  Blocking.kill t;
  print_s [%sexp (name t : string option)];
  [%expect {| () |}];
  require_clean ();
  return ()
;;

let%expect_test "[file_name]" =
  let t = create ~name:"some-name" in
  print_s [%sexp (file_name t : string option)];
  [%expect {| () |}];
  Blocking.kill t;
  print_s [%sexp (file_name t : string option)];
  [%expect {| () |}];
  require_clean ();
  return ()
;;

let%expect_test "[is_live]" =
  let t = find_or_create ~name:"test-buffer" in
  let show () = print_s [%message "" ~is_live:(is_live t : bool)] in
  show ();
  [%expect {| (is_live true) |}];
  Blocking.kill t;
  show ();
  [%expect {| (is_live false) |}];
  require_clean ();
  return ()
;;

let%expect_test "[find] Some" =
  let t = create ~name:"foo" in
  print_s [%sexp (find ~name:"foo" : t option)];
  [%expect {| ("#<buffer foo>") |}];
  Blocking.kill t;
  require_clean ();
  return ()
;;

let%expect_test "[find] None" =
  print_s [%sexp (find ~name:"no buffer with this name" : t option)];
  [%expect {| () |}];
  require_clean ();
  return ()
;;

let%expect_test "[find_exn]" =
  let t = create ~name:"foo" in
  print_s [%sexp (find_exn ~name:"foo" : t)];
  [%expect {| "#<buffer foo>" |}];
  let%bind () = kill t in
  require_does_raise [%here] (fun () -> find_exn ~name:"foo");
  [%expect {| ("no buffer named" foo) |}];
  require_clean ();
  return ()
;;

let%expect_test "[find_or_create]" =
  let f () = find_or_create ~name:"test-buffer" in
  let t = f () in
  show t;
  [%expect {| "#<buffer test-buffer>" |}];
  require [%here] (equal t (f ()));
  Blocking.kill t;
  require_clean ();
  return ()
;;

let%expect_test "[displayed_in]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    let t = Current_buffer.get () in
    let show_displayed_in () = print_s [%sexp (displayed_in t : Window.t list)] in
    show_displayed_in ();
    [%expect {| () |}];
    Selected_window.Blocking.switch_to_buffer t;
    show_displayed_in ();
    [%expect {| ("#<window 1 on *temp-buffer*>") |}];
    Selected_window.split_vertically_exn ();
    show_displayed_in ();
    [%expect {| ("#<window 1 on *temp-buffer*>" "#<window 4 on *temp-buffer*>") |}]);
  return ()
;;

let%expect_test "[display]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    let t = Current_buffer.get () in
    let show_displayed_in () = print_s [%sexp (displayed_in t : Window.t list)] in
    show_displayed_in ();
    [%expect {| () |}];
    print_s [%sexp (display t : Window.t option)];
    [%expect {| ("#<window 4 on *temp-buffer*>") |}];
    show_displayed_in ();
    [%expect {| ("#<window 4 on *temp-buffer*>") |}]);
  return ()
;;

let%expect_test "[buffer_local_value]" =
  let t = create ~name:"b" in
  let var = int_var "v" in
  Var.make_buffer_local_always var;
  Current_buffer.set_value var 13;
  Current_buffer.set_temporarily Sync t ~f:(fun () -> Current_buffer.set_value var 14);
  print_s [%sexp (buffer_local_value (Current_buffer.get ()) var : int)];
  [%expect {| 13 |}];
  print_s [%sexp (buffer_local_value t var : int)];
  [%expect {| 14 |}];
  return ()
;;

let%expect_test "[buffer_local_variables]" =
  let var = int_var "s" in
  Var.make_buffer_local_always var;
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    show_current_buffer_local_variables ();
    [%expect
      {|
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
    [%expect
      {|
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
  return ()
;;

let%expect_test "[find_file_noselect] on a file that exists" =
  let%bind t = find_file_noselect "test_buffer.ml" in
  show t;
  [%expect {| "#<buffer test_buffer.ml>" |}];
  Blocking.kill t;
  return ()
;;

let%expect_test "[find_file_noselect] on a non-existent file" =
  let%bind t = find_file_noselect "zzz" in
  show t;
  [%expect {| "#<buffer zzz>" |}];
  Blocking.kill t;
  return ()
;;

let%expect_test "[save_some]" =
  let restore = unstage (ignore_stderr ()) in
  let save_some ?which_buffers () = save_some () ?which_buffers ~query:false in
  let%bind () = save_some () in
  let file = File.make_temp_file ~prefix:"" ~suffix:".tmp" in
  let basename = Filename.nondirectory file in
  let normalize string = string |> replace_s ~pattern:basename ~with_:"$BASENAME" in
  let is_modified () = print_s [%sexp (Current_buffer.is_modified () : bool)] in
  let%bind () = Selected_window.find_file file in
  Point.insert "foo";
  let%bind () = save_some () in
  is_modified ();
  [%expect {| false |}];
  Current_buffer.erase ();
  let print_buffer_and_return bool b =
    print_s ([%sexp (b : Buffer.t)] |> normalize);
    bool
  in
  let%bind () = save_some () ~which_buffers:(These (print_buffer_and_return false)) in
  [%expect {| "#<buffer $BASENAME>" |}];
  is_modified ();
  [%expect {| true |}];
  let%bind () = save_some () ~which_buffers:(These (print_buffer_and_return true)) in
  [%expect {| "#<buffer $BASENAME>" |}];
  is_modified ();
  [%expect {| false |}];
  let%bind () = Current_buffer.kill () in
  File.delete file;
  restore ();
  return ()
;;

let%expect_test "[revert]" =
  Directory.with_temp_dir Async ~prefix:"test-revert" ~suffix:"" ~f:(fun dir ->
    let%bind () = Selected_window.find_file (concat [ dir; "/file" ]) in
    ignore
      (Process.shell_command_exn "echo foo >file" ~working_directory:Of_current_buffer
       : string);
    print_s [%sexp (Current_buffer.contents () : Text.t)];
    [%expect {| "" |}];
    let%bind () = revert (Current_buffer.get ()) in
    print_s [%sexp (Current_buffer.contents () : Text.t)];
    [%expect {| "foo\n" |}];
    let%bind () = Current_buffer.kill () in
    return ())
;;

let%expect_test "[modified_tick], [chars_modified_tick]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    let t = Current_buffer.get () in
    let tick1 = modified_tick t in
    let chars_tick1 = chars_modified_tick t in
    (* Inserting a character updates both to the same value. *)
    Point.insert "a";
    let tick2 = modified_tick t in
    let chars_tick2 = chars_modified_tick t in
    require [%here] (Modified_tick.( > ) tick2 tick1);
    require [%here] (Modified_tick.( > ) chars_tick2 chars_tick1);
    require [%here] (Modified_tick.( = ) chars_tick2 tick2);
    (* Changing a property updates [modified_tick] but not [chars_modified_tick]. *)
    Current_buffer.set_text_property Text.Property_name.face [];
    let tick3 = modified_tick t in
    let chars_tick3 = chars_modified_tick t in
    require [%here] (Modified_tick.( > ) tick3 tick2);
    require [%here] (Modified_tick.( = ) chars_tick3 chars_tick2));
  [%expect {| |}];
  return ()
;;
