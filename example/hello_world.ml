open! Core_kernel
open! Ecaml

(* To build this file, run
   ocamlfind ocamlopt -linkpkg -package ecaml -thread -output-complete-obj -runtime-variant _pic -pp ppx-jane -o hello_world.so hello_world.mli hello_world.ml

   To test, run
   emacs -Q -L . --batch --eval "(progn (require 'hello-world) (say-hello \"emacs\"))"
*)

let () =
  message "Hello, world!";
  defun [%here] Value.Type.unit ("say-hello" |> Symbol.intern)
    ~docstring:"Takes one argument NAME and says \"Hello, NAME\""
    (let open Defun.Let_syntax in
     let%map_open name = required ("NAME" |> Symbol.intern) Value.Type.string in
     message ("Hello, " ^ name));
  provide ("hello-world" |> Symbol.intern)
;;
