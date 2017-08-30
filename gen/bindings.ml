open! Core_kernel
open! Import

let print_all (g : Generator.t) =
  g "backward-word" [ Int ] Bool;
  g "buffer-live-p" [ Value ] Bool;
  g "elt" ~ocaml_name:"elt_returning_int" [ Value;  Int ] Int;
  g "forward-word"  [ Int ] Bool;
  g "point-max" [] Int;
  g "point-min" [] Int;
;;
