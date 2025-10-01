open! Core
open! Import

let major_mode = Major_mode.wrap_existing "compilation-mode"

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

let find_buffer =
  Funcall.Wrap.("compilation-find-buffer" <: nil_or bool @-> return Buffer.t)
;;

let find_buffer ?avoid_current () = find_buffer avoid_current
