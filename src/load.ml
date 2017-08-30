open! Core_kernel
open! Import

let get_path () =
  Symbol.value_exn Q.load_path
  |> Value.to_list_exn ~f:Value.to_utf8_bytes_exn
;;

let load ?(message = true) filename =
  Symbol.funcall3_i Q.load
    (filename |> Filename.to_value)
    Value.nil
    (not message |> Value.of_bool)
;;
