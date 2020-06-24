open! Core_kernel
open! Import

let substitute_command_keys =
  Funcall.Wrap.("substitute-command-keys" <: string @-> return string)
;;

module Special_sequence = struct
  let keymap symbol = concat [ "\\{"; symbol |> Symbol.name; "}" ]
end
