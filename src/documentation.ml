open! Core
open! Import

let substitute_command_keys =
  Funcall.Wrap.("substitute-command-keys" <: string @-> return string)
;;

let set_advertised_binding symbol key_sequence =
  Symbol.Property.put Symbol.Property.advertised_binding symbol key_sequence
;;

module Special_sequence = struct
  let command symbol = concat [ "\\["; symbol |> Symbol.name; "]" ]
  let keymap symbol = concat [ "\\{"; symbol |> Symbol.name; "}" ]
end
