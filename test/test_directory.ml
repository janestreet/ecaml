open! Core
open! Async_kernel
open! Import
open! Directory

let%expect_test "" =
  (try delete "zzz" ~recursive:true with
   | _ -> ());
  return ()
;;

let%expect_test "[create], [delete]" =
  create "zzz";
  delete "zzz";
  return ()
;;

let%expect_test "[create ~parents:true], [delete ~recursive:true]" =
  create "a/b/c" ~parents:true;
  delete "a" ~recursive:true;
  return ()
;;

let%expect_test "[create] raise" =
  show_raise (fun () -> create "/zzz");
  [%expect
    {| (raised (permission-denied ("Creating directory" "Permission denied" /zzz))) |}];
  return ()
;;

let%expect_test "[delete] raise" =
  print_s
    ~templatize_current_directory:true
    [%sexp (Or_error.try_with (fun () -> delete "zzz") : _ Or_error.t)];
  [%expect
    {|
    (Error (
      file-missing (
        "Removing directory"
        "No such file or directory"
        <current-directory>/zzz)))
    |}];
  return ()
;;

let%expect_test "[files]" =
  create "zzz";
  let show_files () = print_s [%sexp (files "zzz" : Filename.t list)] in
  show_files ();
  [%expect {| () |}];
  touch "zzz/a";
  touch "zzz/b";
  show_files ();
  [%expect {| (a b) |}];
  delete "zzz" ~recursive:true;
  return ()
;;

let%expect_test "[files_recursively]" =
  create "a/b/c" ~parents:true;
  List.iter ~f:touch [ "a/z1"; "a/b/z2"; "a/b/c/z3" ];
  print_s
    [%sexp (files_recursively "a" ~matching:("" |> Regexp.of_pattern) : Filename.t list)];
  [%expect {| (a/b/c/z3 a/b/z2 a/z1) |}];
  delete "a" ~recursive:true;
  return ()
;;
