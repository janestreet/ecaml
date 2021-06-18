open! Core
open! Import

let version_string = Funcall.Wrap.("emacs-version" <: nullary @-> return string)
