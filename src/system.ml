open! Core_kernel
open! Import

let string_option = Value.Type.(option string)

let getenv ~var =
  Symbol.funcall1 Q.getenv (var |> Value.of_utf8_bytes)
  |> string_option.of_value_exn
;;

let setenv ~var ~value =
  Symbol.funcall2_i Q.setenv
    (var |> Value.of_utf8_bytes)
    (value |> string_option.to_value)
;;

let process_environment = Var.create Q.process_environment Value.Type.(list string)

module Var_and_value = struct
  type t =
    { var   : string
    ; value : string }
  [@@deriving sexp_of]
end

let setenv_temporarily vars_and_values ~f =
  let process_environment = Var.create Q.process_environment Value.Type.value in
  Current_buffer.set_value_temporarily ~f
    process_environment
    (Symbol.funcall2 Q.append
       (vars_and_values
        |> List.map ~f:(fun { Var_and_value. var; value } -> concat [ var; "="; value ])
        |> Value.Type.(list string).to_value)
       (Current_buffer.value_exn process_environment));
;;
