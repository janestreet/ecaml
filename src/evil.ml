open! Core_kernel
open! Import
module Current_buffer = Current_buffer0

module Q = struct
  let evil = "evil" |> Symbol.intern
  let evil_declare_ignore_repeat = "evil-declare-ignore-repeat" |> Symbol.intern
  let evilified = "evilified" |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let evil_declare_ignore_repeat =
    Q.evil_declare_ignore_repeat <: Symbol.type_ @-> return ignored
  ;;
end

let declare_ignore_repeat command =
  Eval.after_load [%here] Q.evil ~f:(fun () -> F.evil_declare_ignore_repeat command)
;;

module Config = struct
  type one = Ignore_repeat
  type t = one list

  let apply_one command = function
    | Ignore_repeat -> declare_ignore_repeat command
  ;;

  let apply_to_defun t command = List.iter t ~f:(apply_one command)
end

module State = struct
  type t =
    | Evilified
    | Other of Symbol.t
  [@@deriving equal, sexp_of]

  let type_ =
    Value.Type.map
      Symbol.type_
      ~name:[%sexp "evil-state"]
      ~of_:(fun sym -> if Symbol.equal Q.evilified sym then Evilified else Other sym)
      ~to_:(function
        | Evilified -> Q.evilified
        | Other sym -> sym)
  ;;

  let var = Var.create ("evil-state" |> Symbol.intern) type_
  let get () = Current_buffer.value_exn var
end

module Escape = struct
  let inhibit_functions =
    Var.create
      ("evil-escape-inhibit-functions" |> Symbol.intern)
      Value.Type.(list Function.type_)
  ;;
end
