open! Core_kernel
open! Import

let version_string = Funcall.Wrap.("emacs-version" <: nullary @-> return string)
