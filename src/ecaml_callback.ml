open! Core_kernel
open! Import0

module Value = Value0

module Arity = struct
  type 'callback t =
    | Arity1 : ('a1 ->        'r) t
    | Arity2 : ('a1 -> 'a2 -> 'r) t
  [@@deriving sexp_of]
end

open Arity

type 'callback t =
  { arity : 'callback Arity.t
  ; name  : string
  }
[@@deriving sexp_of]

let register (type callback)
      (t : callback t)
      ~(f : callback) =
  Caml.Callback.register t.name f
;;

let dispatch_function            = { arity = Arity2; name = "dispatch_function"            }
let end_of_module_initialization = { arity = Arity1; name = "end_of_module_initialization" }
let no_active_env                = { arity = Arity1; name = "no_active_env"                }
let free_embedded_caml_values    = { arity = Arity1; name = "free_embedded_caml_values"    }
