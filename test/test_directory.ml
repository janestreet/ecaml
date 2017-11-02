open! Core_kernel
open! Import
open! Directory

let%expect_test "" =
  try delete "zzz" ~recursive:true with _ -> ();
;;

let%expect_test "[create], [delete]" =
  create "zzz";
  delete "zzz";
;;

let%expect_test "[create ~parents:true], [delete ~recursive:true]" =
  create "a/b/c" ~parents:true;
  delete "a" ~recursive:true;
;;

let%expect_test "[create] raise" =
  show_raise (fun () -> create "/zzz");
  [%expect {|
    (raised (file-error ("Creating directory" "Permission denied" /zzz))) |}];
;;

let%expect_test "[delete] raise" =
  print_s ~templatize_current_directory:true
    [%sexp (try_with (fun () -> delete "zzz") : _ Or_error.t)];
  [%expect {|
    (Error (
      file-error (
        "Removing directory"
        "No such file or directory"
        <current-directory>/zzz))) |}]
;;

let%expect_test "[files]" =
  create "zzz";
  let show_files () = print_s [%sexp (files "zzz" : Filename.t list)] in
  show_files ();
  [%expect {|
    () |}];
  touch "zzz/a";
  touch "zzz/b";
  show_files ();
  [%expect {|
    (a b) |}];
  delete "zzz" ~recursive:true;
;;

let%expect_test "[files_recursively]" =
  create "a/b/c" ~parents:true;
  List.iter ~f:touch [ "a/z1"; "a/b/z2"; "a/b/c/z3" ];
  print_s ~templatize_current_directory:true
    [%sexp (files_recursively "a" ~matching:("" |> Regexp.of_pattern) : Filename.t list)];
  [%expect {|
    (<current-directory>/a/b/c/z3
     <current-directory>/a/b/z2
     <current-directory>/a/z1) |}];
  delete "a" ~recursive:true;
;;
