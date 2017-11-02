open! Core_kernel
open! Import
open! File

let show_exists t = print_s [%message "" ~_:(exists t : bool)]

let show t =
  print_s [%message
    ""
      ~exists:(exists t : bool)
      ~is_directory: (try_with (fun () -> is_directory  t) : bool Or_error.t)
      ~is_executable:(try_with (fun () -> is_executable t) : bool Or_error.t)
      ~is_readable:  (try_with (fun () -> is_readable   t) : bool Or_error.t)
      ~is_regular:   (try_with (fun () -> is_regular    t) : bool Or_error.t)
      ~is_symlink:   (try_with (fun () -> is_symlink    t) : bool Or_error.t)
      ~is_writable:  (try_with (fun () -> is_writable   t) : bool Or_error.t)]
;;

let%expect_test "[exists], [is_*]" =
  show "/zzz";
  [%expect {|
    ((exists false)
     (is_directory  (Ok false))
     (is_executable (Ok false))
     (is_readable   (Ok false))
     (is_regular    (Ok false))
     (is_symlink    (Ok false))
     (is_writable   (Ok false))) |}];
  show "test_file.ml";
  [%expect {|
    ((exists true)
     (is_directory  (Ok false))
     (is_executable (Ok false))
     (is_readable   (Ok true))
     (is_regular    (Ok true))
     (is_symlink    (Ok false))
     (is_writable   (Ok true))) |}];
;;

let%expect_test "[is_below]" =
  let dir = Current_buffer.(value_exn directory) in
  let is_below file ~dir = print_s [%sexp (is_below file ~dir : bool)] in
  is_below "foo" ~dir;
  [%expect {|
    true |}];
  is_below "/" ~dir;
  [%expect {|
    false |}];
  is_below "foo" ~dir:(Filename.directory dir |> Option.value_exn);
  [%expect {|
    true |}];
;;

let%expect_test "[copy], [rename], [delete]" =
  copy ~src:"test_file.ml" ~dst:"copy.ml";
  show_exists "test_file.ml";
  [%expect {|
    true |}];
  show_exists "copy.ml";
  [%expect {|
    true |}];
  rename ~src:"copy.ml" ~dst:"rename.ml";
  show_exists "copy.ml";
  [%expect {|
    false |}];
  show_exists "rename.ml";
  [%expect {|
    true |}];
  delete "rename.ml";
  show_exists "rename.ml";
  [%expect {|
    false |}];
;;

let%expect_test "[locate_dominating_file]" =
  Directory.create "a.tmp/b/c" ~parents:true;
  let basename = "foo" in
  touch (concat [ "a.tmp/"; basename ]);
  let test ~above =
    print_s ~templatize_current_directory:true
      [%sexp (locate_dominating_file ~above ~basename
              |> Option.map ~f:File.truename : string option)] in
  test ~above:"a.tmp/b/c";
  [%expect {|
    (<current-directory>/a.tmp/) |}];
  test ~above:"a.tmp/b";
  [%expect {|
    (<current-directory>/a.tmp/) |}];
  test ~above:"a.tmp";
  [%expect {|
    (<current-directory>/a.tmp/) |}];
  Directory.delete "a.tmp" ~recursive:true;
;;

let%expect_test "[locate_dominating_file_exn] raise" =
  show_raise (fun () -> locate_dominating_file_exn ~above:"/" ~basename:"zzz");
  [%expect {|
    (raised "Unable to find [zzz] in directory above [/].") |}];
;;

let%expect_test "[write]" =
  let file = "z.tmp" in
  write file "stuff\n";
  let show_contents () =
    Selected_window.find_file file;
    print_string (Current_buffer.contents () |> Text.to_utf8_bytes);
    Current_buffer.kill () in
  show_contents ();
  [%expect {|
    stuff |}];
  write file "more stuff\n" ~append:true;
  show_contents ();
  [%expect {|
    stuff
    more stuff |}];
  File.delete file;
;;
