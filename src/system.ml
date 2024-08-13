open! Core
open! Import

let getenv = Funcall.Wrap.("getenv" <: string @-> return (nil_or string))
let getenv key = getenv key
let setenv = Funcall.Wrap.("setenv" <: string @-> nil_or string @-> return nil)
let setenv ~key ~data = setenv key data
let process_environment = Var.Wrap.("process-environment" <: list string)
let exec_path = Customization.Wrap.("exec-path" <: path_list)
let noninteractive = Var.Wrap.("noninteractive" <: bool)
let is_interactive () = not (Current_buffer.value_exn noninteractive)

module Var_and_value = struct
  type t =
    { key : string
    ; data : string
    }
  [@@deriving sexp_of]
end

let append = Funcall.Wrap.("append" <: list string @-> value @-> return value)

let setenv_temporarily sync_or_async vars_and_values ~f =
  let process_environment = Var.Wrap.("process-environment" <: value) in
  Current_buffer.set_value_temporarily
    sync_or_async
    ~f
    process_environment
    (append
       (vars_and_values
        |> List.map ~f:(fun { Var_and_value.key; data } -> [%string "%{key}=%{data}"]))
       (Current_buffer.value_exn process_environment))
;;

let hostname = Funcall.Wrap.("system-name" <: nullary @-> return string)
