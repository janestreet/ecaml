open! Core_kernel
open! Import
open! Filename

let%expect_test "[directory], [nondirectory]" =
  List.iter
    [ ""
    ; "a"
    ; "/"
    ; "/a"
    ; "/a/"
    ; "/a/b" ]
    ~f:(fun filename ->
      print_s [%message
        ""
          (filename : string)
          ~directory:   (directory    filename : string option)
          ~nondirectory:(nondirectory filename : string)]);
  [%expect {|
    ((filename "") (directory ()) (nondirectory ""))
    ((filename a) (directory ()) (nondirectory a))
    ((filename /) (directory (/)) (nondirectory ""))
    ((filename /a) (directory (/)) (nondirectory a))
    ((filename /a/) (directory (/a/)) (nondirectory ""))
    ((filename /a/b) (directory (/a/)) (nondirectory b)) |}]
;;

let%expect_test "[directory_exn]" =
  show_raise (fun () -> directory_exn "a");
  [%expect {|
    (raised (
      "[Filename.directory_exn] of filename that has no directory" (filename a))) |}];
;;

let%expect_test "[extension], [sans_extension]" =
  List.iter
    [ "foo"
    ; "foo.ml" ]
    ~f:(fun filename ->
      print_s [%message
        ""
          (filename : string)
          ~extension:(try_with (fun () -> extension_exn filename) : string Or_error.t)
          ~sans_extension:(try_with (fun () -> sans_extension filename ) : string Or_error.t)]);
  [%expect {|
    ((filename foo)
     (extension (Error (wrong-type-argument (stringp nil))))
     (sans_extension (Ok foo)))
    ((filename foo.ml)
     (extension      (Ok ml))
     (sans_extension (Ok foo))) |}]
;;

let%expect_test "[of_directory], [to_directory]" =
  List.iter
    [ "/"
    ; "/a"
    ; "/a/" ]
    ~f:(fun filename ->
      print_s [%message
        ""
          (filename : string)
          ~of_directory:(of_directory filename)
          ~to_directory:(to_directory filename)]);
  [%expect {|
    ((filename     /)
     (of_directory /)
     (to_directory /))
    ((filename     /a)
     (of_directory /a)
     (to_directory /a/))
    ((filename     /a/)
     (of_directory /a)
     (to_directory /a/)) |}];
;;

let%expect_test "[is_absolute]" =
  List.iter
    [ ""
    ; "/"
    ; "."
    ; "./"
    ; "~"
    ; "a"
    ; "/a/" ]
    ~f:(fun filename ->
      print_s [%message
        ""
          (filename : string)
          ~is_absolute:(is_absolute filename : bool)]);
  [%expect {|
    ((filename    "")
     (is_absolute false))
    ((filename    /)
     (is_absolute true))
    ((filename    .)
     (is_absolute false))
    ((filename    ./)
     (is_absolute false))
    ((filename    ~)
     (is_absolute true))
    ((filename    a)
     (is_absolute false))
    ((filename    /a/)
     (is_absolute true)) |}];
;;

let%expect_test "[make_relative]" =
  List.iter
    [ "/a/b/c", "/a/b/c"
    ; "/a/b/c", "/a/b"
    ; "/a/b/c", "/a"
    ; "/a/b/c", "/" ]
    ~f:(fun (filename, relative_to) ->
      print_s [%message
        ""
          (filename : string)
          (relative_to : string)
          ~relative:(make_relative filename ~relative_to)]);
  [%expect {|
    ((filename    /a/b/c)
     (relative_to /a/b/c)
     (relative    .))
    ((filename    /a/b/c)
     (relative_to /a/b)
     (relative    c))
    ((filename    /a/b/c)
     (relative_to /a)
     (relative    b/c))
    ((filename    /a/b/c)
     (relative_to /)
     (relative    a/b/c)) |}];
;;

let%expect_test "[expand]" =
  List.iter
    [ "/", "a"
    ; "/a", "../b"
    ; "/a/b", "c/../d" ]
    ~f:(fun (in_dir, filename) ->
      print_s [%message
        ""
          (in_dir : string)
          (filename : string)
          ~expanded:(expand filename ~in_dir : string)]);
  [%expect {|
    ((in_dir   /)
     (filename a)
     (expanded /a))
    ((in_dir   /a)
     (filename ../b)
     (expanded /b))
    ((in_dir   /a/b)
     (filename c/../d)
     (expanded /a/b/d)) |}];
;;
