open! Core_kernel
open! Import

(** [(describe-function 'defvar)]
    [(Info-goto-node "(elisp)Defining Variables")] *)
val defvar
  :  Symbol.t
  -> Source_code_position.t
  -> docstring:string
  -> type_:'a Value.Type.t
  -> initial_value:'a
  -> ?include_in_all_defvar_symbols:bool (** default is [true] *)
  -> unit
  -> 'a Var.t

(** [(describe-function 'defvaralias)]
    [(Info-goto-node "(elisp)Variable Aliases")] *)
val defvaralias
  :  Symbol.t
  -> Source_code_position.t
  -> ?docstring:string
  -> alias_of:Symbol.t
  -> unit
  -> unit

(** [(describe-function 'define-obsolete-variable-alias)] *)
val define_obsolete_alias
  :  Symbol.t
  -> Source_code_position.t
  -> ?docstring:string
  -> alias_of:Symbol.t
  -> since:string
  -> unit
  -> unit

module Private : sig
  val all_defvar_symbols : unit -> Symbol.t list
end
