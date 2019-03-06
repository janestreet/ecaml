open! Core_kernel
open! Import

module Q = struct
  include Q

  let compilation_buffer_name_function =
    "compilation-buffer-name-function" |> Symbol.intern
  and compilation_next_error = "compilation-next-error" |> Symbol.intern
end

module F = struct
  open Funcall

  let compilation_next_error =
    Q.compilation_next_error
    <: int @-> nil_or bool @-> nil_or Position.type_ @-> return nil
  ;;
end

let buffer_name_function =
  Var.create Q.compilation_buffer_name_function (Value.Type.nil_or Function.type_)
;;

let next_error ?different_file ?start_at nth =
  F.compilation_next_error nth different_file start_at
;;
