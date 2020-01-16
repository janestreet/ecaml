(** [(describe-variable 'auto-mode-alist)]
    [(Info-goto-node "(elisp)Auto Major Mode")] *)

open! Core_kernel
open! Import

module Entry : sig
  type t =
    { delete_suffix_and_recur : bool
    ; (* [filename_match] is an unanchored regexp matching the absolute path name of the
         file.  If you want to match on a suffix, then you must anchor the regexp
         yourself, e.g. using [Rx.(Line End)].*)
      filename_match : Regexp.t
    ; function_ : Symbol.t option
    }
  [@@deriving sexp_of]
end

type t = Entry.t list [@@deriving sexp_of]

val auto_mode_alist : t Var.t
val add : Entry.t list -> unit
