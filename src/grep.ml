open! Core
open! Import

let last_buffer = Var.Wrap.("grep-last-buffer" <: nil_or Buffer.t)

let compilation_start =
  Funcall.Wrap.("compilation-start" <: string @-> Symbol.t @-> return nil)
;;

let major_mode = Major_mode.wrap_existing "grep-mode"
let grep ~command = compilation_start command (Major_mode.symbol major_mode)
