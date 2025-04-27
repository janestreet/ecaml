open! Core
open! Import
open! Ecaml_filename

let path = Var.Wrap.("load-path" <: path_list)

let load =
  let load = Funcall.Wrap.("load" <: Filename.t @-> bool @-> bool @-> return nil) in
  fun ?(message = true) filename ->
    Value.Private.run_outside_async (fun () -> load filename false (not message))
;;
