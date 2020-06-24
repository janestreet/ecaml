open! Core_kernel
open! Import

let buffer_name_function =
  Var.Wrap.("compilation-buffer-name-function" <: nil_or Function.t)
;;

let compilation_next_error =
  Funcall.Wrap.(
    "compilation-next-error" <: int @-> nil_or bool @-> nil_or Position.t @-> return nil)
;;

let next_error ?different_file ?start_at nth =
  compilation_next_error nth different_file start_at
;;
