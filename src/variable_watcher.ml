open! Core_kernel
open! Import

module Operation = struct
  module T = struct
    type t =
      | Set
      | Let
      | Unlet
      | Makunbound
      | Defvaralias
    [@@deriving enumerate, sexp_of]
  end

  include T

  let type_ =
    Value.Type.enum
      [%message "variable-change-operation"]
      (module T)
      ((function
         | Set -> "set"
         | Let -> "let"
         | Unlet -> "unlet"
         | Makunbound -> "makunbound"
         | Defvaralias -> "defvaralias")
       >> Symbol.intern
       >> Symbol.to_value)
  ;;
end

module Event = struct
  type t =
    { local_to_buffer : Buffer.t option
    ; new_value : Value.t
    ; operation : Operation.t
    ; variable_changed : Symbol.t
    }
  [@@deriving sexp_of]
end

let build_function here f =
  Defun.lambda
    here
    (Returns Value.Type.unit)
    (let%map_open.Defun () = return ()
     and symbol = required "symbol" Symbol.type_
     and newval = required "newval" Value.Type.value
     and operation = required "operation" Operation.type_
     and where = required "where" (Value.Type.option Buffer.type_) in
     f
       { Event.local_to_buffer = where
       ; new_value = newval
       ; operation
       ; variable_changed = symbol
       })
;;

type t =
  { symbol : Symbol.t
  ; watcher : Function.t
  }

let add_variable_watcher =
  Funcall.Wrap.("add-variable-watcher" <: Symbol.type_ @-> Function.type_ @-> return nil)
;;

let add here var ~f =
  let watcher = build_function here f in
  add_variable_watcher (Var.symbol var) watcher;
  { symbol = Var.symbol var; watcher }
;;

let remove_variable_watcher =
  Funcall.Wrap.(
    "remove-variable-watcher" <: Symbol.type_ @-> Function.type_ @-> return nil)
;;

let remove { symbol; watcher } = remove_variable_watcher symbol watcher
