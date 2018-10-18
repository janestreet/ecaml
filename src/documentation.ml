open! Core_kernel
open! Import

module Q = struct
  let substitute_command_keys = "substitute-command-keys" |> Symbol.intern
end

module F = struct
  open Funcall
  open Value.Type

  let substitute_command_keys = Q.substitute_command_keys <: string @-> return string
end

let substitute_command_keys = F.substitute_command_keys
