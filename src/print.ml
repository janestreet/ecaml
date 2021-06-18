open! Core
open! Import

let length = Var.Wrap.("print-length" <: nil_or int)
let level = Var.Wrap.("print-level" <: nil_or int)
