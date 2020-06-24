open! Core_kernel
open! Async_kernel
open! Import

let save_abbrevs = Customization.Wrap.("save-abbrevs" <: bool)
