open! Core_kernel
open! Import
open! Load

let%expect_test "[load_path]" =
  print_s [%sexp (List.tl_exn (path ()) : string list)];
  [%expect {|
    (/j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/vc
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/url
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/textmodes
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/progmodes
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/play
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/org
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/nxml
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/net
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/mh-e
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/mail
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/leim
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/language
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/international
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/gnus
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/eshell
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/erc
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/emulation
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/emacs-lisp
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/cedet
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/calendar
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/calc
     /j/office/app/emacs/builds/25.2/share/emacs/25.2/lisp/obsolete) |}];
;;

let%expect_test "[load]" =
  let file = Caml.Filename.temp_file "" "" in
  Selected_window.find_file file;
  Point.insert "(setq zzz 13)";
  Current_buffer.save ();
  Current_buffer.kill ();
  load file ~message:false;
  print_s [%sexp (Current_buffer.value_exn (Var.create ("zzz" |> Symbol.intern) Value.Type.int) : int)];
  [%expect {|
    13 |}];
;;
