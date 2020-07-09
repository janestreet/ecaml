(** [(Info-goto-node "(elisp)Debugger")] *)

open! Core_kernel
open! Import

(** [(describe-variable 'debug-on-error)]
    [(Info-goto-node "(elisp)Error Debugging")] *)
val debug_on_error : bool Customization.t

val toggle_debug_on_error_symbol : Symbol.t
