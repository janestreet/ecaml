open! Core
open! Import
include (val Major_mode.wrap_existing_with_lazy_keymap "dired-mode")

let get_marked_files =
  Funcall.Wrap.(
    "dired-get-marked-files"
    <: nullary @-> return (list Ecaml_filename.Filename.absolute_t))
;;
