open! Core
open! Import

let substitute_command_keys =
  Funcall.Wrap.("substitute-command-keys" <: Text.t @-> return Text.t)
;;

let set_advertised_binding symbol key_sequence =
  Symbol.Property.put Symbol.Property.advertised_binding symbol key_sequence
;;

module Special_sequence = struct
  let command symbol = [%string "\\[%{Symbol.name symbol}]"]
  let symbol symbol = [%string "`%{Symbol.name symbol}'"]
  let customization customization = customization |> Customization.symbol |> symbol
  let describe_keymap symbol = [%string "\\{%{Symbol.name symbol}}"]
  let assume_keymap symbol = [%string "\\<%{Symbol.name symbol}>"]
end

module O = struct
  let substitute_command_keys = Text.of_utf8_bytes >> substitute_command_keys

  include Special_sequence
end
