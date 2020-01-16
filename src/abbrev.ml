open! Core_kernel
open! Async_kernel
open! Import

let save_abbrevs = Var.Wrap.("save-abbrevs" <: bool)
