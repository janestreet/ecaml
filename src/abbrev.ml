open! Core
open! Async_kernel
open! Import

let save_abbrevs = Customization.Wrap.("save-abbrevs" <: bool)
