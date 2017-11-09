open! Core_kernel
open! Import
open! Hook

let t = create Normal ("some-hook" |> Symbol.intern)
let () = clear t

let show t = print_s [%sexp (t : _ t)]

let create_function s =
  Function.create [%here] Normal (s |> Symbol.intern) (fun () -> print_s [%message s])
;;

let f1 = create_function "f1"
let f2 = create_function "f2"
let f3 = create_function "f3"

let%expect_test "[add]" =
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value ())) |}];
  add t f1;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value (f1))) |}];
  add t f2;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value (f2 f1))) |}];
  add t f3 ~where:End;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value (f2 f1 f3))) |}];
  clear t;
;;

let%expect_test "[add] when present" =
  add t f1;
  add t f1;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value (f1))) |}];
  clear t;
;;

let%expect_test "[remove]" =
  add t f2;
  add t f1;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value (f1 f2))) |}];
  remove t f2;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value (f1))) |}];
  remove t f1;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value ())) |}];
  clear t;
;;

let%expect_test "[remove] when absent" =
  remove t f1;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value ())) |}];
  add t f2;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value (f2))) |}];
  remove t f1;
  show t;
  [%expect {|
    ((symbol some-hook)
     (type_  Normal)
     (value (f2))) |}];
  clear t;
;;

let%expect_test "[run]" =
  run t;
  add t f1;
  run t;
  [%expect {|
    f1 |}];
  add t f2;
  run t;
  [%expect {|
    f2
    f1 |}];
;;

let create_after_load_fun s =
  Function.create [%here] File (s |> Symbol.intern) (fun ~file:_ -> print_s [%message s])
;;

let%expect_test "[after_load] hooks" =
  let f1 = create_after_load_fun "f1" in
  let f2 = create_after_load_fun "f2" in
  add after_load f1;
  after_load_once (fun ~file:_ -> print_endline "after_load_once hook");
  add after_load f2;
  let file = Caml.Filename.temp_file "ecamltest" ".el" in
  Out_channel.write_all file ~data:"'()";
  Load.load ~message:false file;
  [%expect {|
    f2
    after_load_once hook
    f1 |}];
  Load.load ~message:false file;
  [%expect {|
    f2
    f1 |}];
  remove after_load f1;
  remove after_load f2;
;;

let%expect_test "[after_save], [kill_buffer]" =
  let file = "test-after-save.tmp" in
  Selected_window.find_file file;
  add after_save ~buffer_local:true
    (Function.create [%here] Normal ("test-after-save-hook" |> Symbol.intern)
       (fun () -> print_s [%message "after-save hook ran"]));
  print_s [%sexp (Current_buffer.is_buffer_local (var after_save) : bool)];
  [%expect {|
    true |}];
  add kill_buffer ~buffer_local:true
    (Function.create [%here] Normal ("test-kill-buffer-hook" |> Symbol.intern)
       (fun () -> print_s [%message "kill-buffer hook ran"]));
  Point.insert "foo";
  Current_buffer.save ();
  [%expect {|
    "after-save hook ran" |}];
  Current_buffer.kill ();
  [%expect {|
    "kill-buffer hook ran" |}];
  File.delete file;
;;
