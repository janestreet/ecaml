open! Core_kernel
open! Import
open! Obarray

let%expect_test "[iter], [standard]" =
  let all = ref [] in
  iter standard ~f:(fun s -> all := Symbol.name s :: !all);
  print_s [%sexp (List.length !all : int)];
  [%expect {|
    15_262 |}];
  print_s [%sexp (!all |> List.sort ~compare:String.compare |> (fun l -> List.take l 100)
                  : string list)];
  [%expect {|
    (%
     &context
     &define
     &key
     &optional
     &or
     &rest
     *
     **
     +
     ,
     ,.
     ,@
     -
     --dolist-tail--
     --dotimes-counter--
     --dotimes-limit--
     -with-timeout-timer-
     -with-timeout-value-
     ...
     /
     /=
     1+
     1-
     1value
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
     :abbrev-table-modiff
     :adstyle
     :advertised-binding
     :after
     :after-until
     :after-while
     :align-to
     :all
     :allow-other-keys
     :animate-buffer
     :annotation-function
     :antialias
     :append
     :argument-precedence-order
     :around
     :ascent
     :ascii-compatible-p
     :autohint
     :autoload-end
     :avgwidth
     :background
     :base
     :before
     :before-until
     :before-while
     :bindtodevice
     :bold
     :bom
     :box
     :broadcast
     :buffer
     :button
     :bytesize
     :case-fixed
     :category
     :ccl-decoder
     :ccl-encoder
     :charset-list
     :cl--generic--under-construction
     :code-offset
     :code-space
     :coding
     :coding-type
     :color
     :color-adjustment
     :color-symbols
     :combining-capability
     :command
     :company-doc-buffer
     :company-docsig
     :company-location
     :completions
     :connection-type
     :conversion
     :count
     :crlfiles
     :crop
     :data
     :decode-translation-table
     :default-char) |}];
;;
