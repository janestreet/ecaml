open! Core
open! Import

let major_mode = Major_mode.wrap_existing "dired-mode"

let get_marked_files =
  Funcall.Wrap.(
    "dired-get-marked-files"
    <: nullary @-> return (list Ecaml_filename.Filename.absolute_t))
;;
