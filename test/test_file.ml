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
