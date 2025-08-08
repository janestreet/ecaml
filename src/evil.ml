open! Core
open! Import
module Current_buffer = Current_buffer0

module Q = struct
  let evilified = "evilified" |> Symbol.intern
  let motion = "motion" |> Symbol.intern
  let normal = "normal" |> Symbol.intern
end

let evil_local_mode = Var.Wrap.("evil-local-mode" <: bool)
let is_in_use () = Current_buffer.has_non_null_value evil_local_mode
let evil_mode = Var.Wrap.("evil-mode" <: bool)
let is_in_use_globally () = Current_buffer.has_non_null_value evil_mode

module State = struct
  type t =
    | Evilified
    | Motion
    | Normal
    | Other of Symbol.t
  [@@deriving equal, sexp_of]

  let to_symbol = function
    | Evilified -> Q.evilified
    | Motion -> Q.motion
    | Normal -> Q.normal
    | Other sym -> sym
  ;;

  let type_ =
    Value.Type.map
      Symbol.t
      ~name:[%sexp "evil-state"]
      ~of_:(fun sym ->
        if Symbol.equal Q.evilified sym
        then Evilified
        else if Symbol.equal Q.motion sym
        then Motion
        else if Symbol.equal Q.normal sym
        then Normal
        else Other sym)
      ~to_:to_symbol
  ;;

  let t = type_

  let is_active t =
    let name = t |> to_symbol |> Symbol.name in
    let predicate_name = [%string "evil-%{name}-state-p"] in
    let f = Funcall.Wrap.(predicate_name <: nullary @-> return bool) in
    f ()
  ;;

  let activate t =
    let name = t |> to_symbol |> Symbol.name in
    let enable_name = [%string "evil-%{name}-state"] in
    let f = Funcall.Wrap.(enable_name <: nullary @-> return ignored) in
    f ()
  ;;

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
