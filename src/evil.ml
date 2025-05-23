open! Core
open! Import
module Current_buffer = Current_buffer0

module Q = struct
  let evil = "evil" |> Symbol.intern
  let evil_mode = "evil-mode" |> Symbol.intern
  let evilified = "evilified" |> Symbol.intern
  let normal = "normal" |> Symbol.intern
end

let is_in_use () =
  Current_buffer.has_non_null_value { symbol = Q.evil_mode; type_ = Value.Type.bool }
;;

let evil_declare_ignore_repeat =
  Funcall.Wrap.("evil-declare-ignore-repeat" <: Symbol.t @-> return ignored)
;;

let declare_ignore_repeat command =
  Eval.after_load Q.evil ~f:(fun () -> evil_declare_ignore_repeat command)
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
    | Normal
    | Other of Symbol.t
  [@@deriving equal, sexp_of]

  let type_ =
    Value.Type.map
      Symbol.t
      ~name:[%sexp "evil-state"]
      ~of_:(fun sym ->
        if Symbol.equal Q.evilified sym
        then Evilified
        else if Symbol.equal Q.normal sym
        then Normal
        else Other sym)
      ~to_:(function
        | Evilified -> Q.evilified
        | Normal -> Q.normal
        | Other sym -> sym)
  ;;

  let t = type_
  let var = Var.Wrap.("evil-state" <: t)
  let get () = Current_buffer.value_exn var

  let insert =
    let evil_insert = Funcall.Wrap.("evil-insert" <: value @-> return nil) in
    fun () -> evil_insert Value.nil
  ;;
end

module Escape = struct
  let inhibit_functions = Var.Wrap.("evil-escape-inhibit-functions" <: list Function.t)
end

let define_key =
  Funcall.Wrap.(
    "evil-define-key*"
    <: list State.t @-> Keymap.t @-> Key_sequence.t @-> Keymap.Entry.t @-> return nil)
;;

let evil_visual_refresh =
  Funcall.Wrap.("evil-visual-refresh" <: nullary @-> return ignored)
;;

let evil_visual_state_p = Funcall.Wrap.("evil-visual-state-p" <: nullary @-> return bool)

let visual_refresh_if_necessary () =
  if is_in_use () && evil_visual_state_p () then evil_visual_refresh ()
;;
