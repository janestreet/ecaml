open! Core_kernel
open! Import

module Q = struct
  include Q
  let load                             = "load"                             |> Symbol.intern
  let load_path                        = "load-path"                        |> Symbol.intern
end

let load_path = Var.create Q.load_path Value.Type.path_list

let path () = Current_buffer.value_exn load_path

let load ?(message = true) filename =
  Symbol.funcall3_i Q.load
    (filename |> Filename.to_value)
    Value.nil
    (not message |> Value.of_bool)
;;
