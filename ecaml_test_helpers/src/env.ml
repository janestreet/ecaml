open! Core
open! Async
open! Import

(* Private_fe environment variables must be set both in the Unix process environment (used
   by OCaml), and in emacs's [process-environment] variable. *)
let putenv ~key ~data =
  (Unix.putenv [@ocaml.alert "-unsafe_multidomain"]) ~key ~data;
  System.setenv ~key ~data:(Some data)
;;

let unsetenv var =
  (Unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) var;
  System.setenv ~key:var ~data:None
;;

let getenv key = System.getenv key
let getenv_exn key = getenv key |> Option.value_exn
