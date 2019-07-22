open! Core_kernel
open! Import
open! Ecaml_filename

let load_path = Var.Wrap.("load-path" <: path_list)
let path () = Current_buffer.value_exn load_path
let load = Funcall.("load" <: Filename.t @-> bool @-> bool @-> return nil)

let load ?(message = true) filename =
  Value.Private.run_outside_async [%here] (fun () -> load filename false (not message))
;;
