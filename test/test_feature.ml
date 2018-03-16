open! Core_kernel
open! Import
open! Feature

let foo = "foo" |> Symbol.intern

let%expect_test "[require] raise" =
  show_raise (fun () -> require foo);
  [%expect {|
    (raised (file-error ("Cannot open load file" "No such file or directory" foo))) |}];
;;

let%expect_test "[provide], [require]" =
  provide foo;
  require foo;
;;

let%expect_test "[require] uses [Symbol.equal]" =
  show_raise (fun () -> require (Symbol.create ~name:"foo"));
  [%expect {|
    (raised (file-error ("Cannot open load file" "No such file or directory" foo))) |}];
;;

let%expect_test "[all_provided]" =
  print_s [%sexp (all_provided ()
                  |> List.sort ~compare:(fun t1 t2 ->
                    String.compare (Symbol.name t1) (Symbol.name t2))
                  : Symbol.t list)];
  [%expect {|
    (abbrev
     backquote
     base64
     button
     case-table
     cham
     charscript
     chinese
     cl-generic
     cl-preloaded
     code-pages
     cp51932
     cus-face
     custom
     cyrillic
     czech
     dnd
     dynamic-setting
     ediff-hook
     eldoc
     electric
     elisp-mode
     emacs
     env
     epa-hook
     ethiopic
     eucjp-ms
     european
     facemenu
     faces
     files
     find-func
     font-core
     font-lock
     font-render-setting
     fontset
     foo
     format
     frame
     fringe
     georgian
     greek
     hashtable-print-readable
     hebrew
     help
     image
     indian
     inotify
     japanese
     jit-lock
     jka-cmpr-hook
     korean
     lao
     lisp-float-type
     lisp-mode
     loaddefs
     macroexp
     make-network-process
     md5
     menu-bar
     minibuffer
     misc-lang
     mouse
     mule
     mule-util
     multi-tty
     mwheel
     nadvice
     newcomment
     overlay
     page
     prog-mode
     regexp-opt
     register
     rfn-eshadow
     romanian
     scroll-bar
     select
     sha1
     simple
     slovak
     syntax
     tabulated-list
     tai-viet
     term/common-win
     text-properties
     thai
     thingatpt
     tibetan
     timer
     tool-bar
     tooltip
     uniquify
     utf-8-lang
     vc-hooks
     vietnamese
     widget
     x
     x-dnd
     x-toolkit
     x-win) |}];
;;

let%expect_test "[is_provided]" =
  print_s [%sexp (is_provided ("abbrev" |> Symbol.intern) : bool)];
  [%expect {|
    true |}];
  print_s [%sexp (is_provided ("zzz" |> Symbol.intern) : bool)];
  [%expect {|
    false |}];
;;
