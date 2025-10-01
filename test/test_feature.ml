open! Core
open! Async_kernel
open! Import
open! Feature

let foo = "foo" |> Symbol.intern

let%expect_test "[require] raise" =
  show_raise (fun () -> require foo);
  [%expect
    {|
    (raised (
      file-missing ("Cannot open load file" "No such file or directory" foo)))
    |}];
  return ()
;;

let%expect_test "[provide], [require]" =
  Ecaml.provide foo;
  require foo;
  return ()
;;

let%expect_test "[require] uses [Symbol.equal]" =
  show_raise (fun () -> require (Symbol.create_uninterned ~name:"foo"));
  [%expect
    {|
    (raised (
      file-missing ("Cannot open load file" "No such file or directory" foo)))
    |}];
  return ()
;;

let%expect_test "[all_provided]" =
  print_s
    [%sexp
      (all_provided ()
       |> List.sort ~compare:(fun t1 t2 ->
         String.compare (Symbol.name t1) (Symbol.name t2))
       : Symbol.t list)];
  [%expect
    {|
    (abbrev
     ansi-color
     backquote
     base64
     bookmark
     button
     cairo
     case-table
     cconv
     cham
     charprop
     charscript
     chinese
     cl-generic
     cl-lib
     cl-loaddefs
     cl-preloaded
     code-pages
     composite
     cp51932
     cus-face
     custom
     cyrillic
     czech
     dbusbind
     derived
     dnd
     dynamic-setting
     easymenu
     ediff-hook
     eldoc
     electric
     elisp-mode
     emacs
     emacs-inline-tests-runner
     emoji-zwj
     env
     epa-hook
     ethiopic
     eucjp-ms
     european
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
     indonesian
     inotify
     isearch
     iso-transl
     japanese
     jit-lock
     jka-cmpr-hook
     keymap
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
     move-toolbar
     mule
     multi-tty
     mwheel
     nadvice
     native-compile
     newcomment
     obarray
     oclosure
     overlay
     page
     paren
     philippine
     pp
     prog-mode
     regexp-opt
     register
     replace
     rfn-eshadow
     rmc
     romanian
     scroll-bar
     select
     seq
     sha1
     simple
     slovak
     subr-x
     syntax
     system-font-setting
     tab-bar
     tabulated-list
     tai-viet
     term/common-win
     term/tty-colors
     term/x-win
     text-mode
     text-properties
     text-property-search
     thai
     theme-loaddefs
     threads
     tibetan
     timer
     tool-bar
     tooltip
     touch-screen
     uniquify
     utf-8-lang
     vc-hooks
     vietnamese
     widget
     window
     x
     x-dnd
     x-toolkit
     x-win
     xinput2)
    |}];
  return ()
;;

let%expect_test "[is_provided]" =
  print_s [%sexp (is_provided ("abbrev" |> Symbol.intern) : bool)];
  [%expect {| true |}];
  print_s [%sexp (is_provided ("zzz" |> Symbol.intern) : bool)];
  [%expect {| false |}];
  return ()
;;
