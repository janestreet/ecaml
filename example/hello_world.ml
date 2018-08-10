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
    [%here]
    Value.Type.unit
    ("say-hello" |> Symbol.intern)
    ~docstring:"Takes one argument NAME and says \"Hello, NAME\""
    (let open Defun.Let_syntax in
     let%map_open name = required ("NAME" |> Symbol.intern) Value.Type.string in
     message ("Hello, " ^ name));
  provide ("hello_world" |> Symbol.intern)
;;
