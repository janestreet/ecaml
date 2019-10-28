open! Core_kernel
open! Import

let version_string = Funcall.("emacs-version" <: nullary @-> return string)
