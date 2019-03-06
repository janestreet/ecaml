open! Core_kernel
open! Import

module Q = struct
  let evil = "evil" |> Symbol.intern
  let evil_declare_ignore_repeat = "evil-declare-ignore-repeat" |> Symbol.intern
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
