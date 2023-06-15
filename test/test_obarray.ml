open! Core
open! Async_kernel
open! Import
open! Obarray

let%expect_test "[iter], [standard]" =
  let all = ref [] in
  iter standard ~f:(fun s -> all := Symbol.name s :: !all);
  (* We ignore the last two digits of the length to reduce noise. *)
  print_s [%sexp (List.length !all / 100 * 100 : int)];
  [%expect {|
    17_600 |}];
  print_s
    [%sexp
      (!all |> List.sort ~compare:String.compare |> fun l -> List.take l 100
                                                             : string list)];
  [%expect
    {|
    (%
     &context
     &define
     &interpose
     &key
     &name
     &optional
     &or
     &rest
     "(setf seq-elt)"
     *
     **
     +
     ,
     ,@
     -
     --displaying-byte-compile-warnings-fn
     --dolist-tail--
     --dotimes-counter--
     --dotimes-limit--
     -with-timeout-timer-
     -with-timeout-value-
     ...
     /
     /=
     0-1
     1+
     1-
     1-2
     1-3
     1value
     2-3
     2-and
     2C-associate-buffer
     2C-command
     2C-split
     2C-two-columns
     3FR
     5x5
     5x5-crack
     5x5-crack-mutating-best
     5x5-crack-mutating-current
     5x5-crack-randomly
     5x5-crack-xor-mutate
     7-bit
     8-bit-level-4
     :
     :Tag
     :abbrev-table
     :abbrev-table-modiff
     :active
     :adstyle
     :advertised-binding
     :affixation-function
     :after
     :after-hook
     :after-until
     :after-while
     :align-to
     :all
     :allow-other-keys
     :animate-buffer
     :animate-multi-frame-data
     :animate-tardiness
     :annotation-function
     :antialias
     :append
     :application
     :argument-precedence-order
     :around
     :array
     :array-type
     :ascent
     :ascii-compatible-p
     :autohint
     :autoload-end
     :avgwidth
     :background
     :base
     :base-uri
     :before
     :before-until
     :before-while
     :beginning
     :bindtodevice
     :bold
     :bom
     :boolean
     :box
     :broadcast
     :buffer
     :button
     :byte
     :byte-func
     :bytesize
     :c-name
     :captured+mutated
     :case-fixed
     :category
     :ccl-decoder) |}];
  return ()
;;
