open! Core_kernel
open! Ecaml

(* To build this file, run
   dune build hello_world.so

   To test, run
   emacs -Q -L _build/default --batch --eval "(progn (require 'hello_world) (say-hello \"emacs\"))"
*)

let () =
  message "Hello, world!";
  defun
    ("say-hello" |> Symbol.intern)
    [%here]
    ~docstring:{|
Takes one argument NAME and says "Hello, NAME"
|}
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open name = required "name" string in
     message ("Hello, " ^ name));
  provide ("hello_world" |> Symbol.intern)
;;
