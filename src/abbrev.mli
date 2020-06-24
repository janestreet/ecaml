(** Abbrevs are words which expand into other text when typed in.

    - [(Info-goto-node "(emacs)Abbrevs")] *)

open! Core_kernel
open! Async_kernel
open! Import

(** [(describe-variable 'save-abbrevs)] *)
val save_abbrevs : bool Customization.t
