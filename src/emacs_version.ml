open! Core
open! Import

let version_string = Funcall.Wrap.("emacs-version" <: nullary @-> return string)
let major_version = Current_buffer.value_exn Var.Wrap.("emacs-major-version" <: int)
