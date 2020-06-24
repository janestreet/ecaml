open! Core_kernel
open! Import

module Q = struct
  include Q

  let undefined = "undefined" |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "keymap"
    let here = [%here]
    let is_in_subtype = Value.is_keymap
  end)

type keymap = t [@@deriving sexp_of]

let equal = eq
let parent = Funcall.Wrap.("keymap-parent" <: t @-> return (nil_or t))
let set_parent = Funcall.Wrap.("set-keymap-parent" <: t @-> nil_or t @-> return nil)
let set_transient = Funcall.Wrap.("set-transient-map" <: t @-> bool @-> return nil)
let set_transient ?(keep_if_used = false) t = set_transient t keep_if_used

module Kind = struct
  type t =
    | Full
    | Sparse
  [@@deriving sexp_of]
end

let make_keymap = Funcall.Wrap.("make-keymap" <: nil_or string @-> return t)

let make_sparse_keymap =
  Funcall.Wrap.("make-sparse-keymap" <: nil_or string @-> return t)
;;

let create ?(kind = Kind.Sparse) ?menu_name () =
  (match kind with
   | Full -> make_keymap
   | Sparse -> make_sparse_keymap)
    menu_name
;;

let deep_copy = Funcall.Wrap.("copy-keymap" <: t @-> return t)
let global = Funcall.Wrap.("current-global-map" <: nullary @-> return t)
let set_global = Funcall.Wrap.("use-global-map" <: t @-> return nil)

module Entry = struct
  type t =
    | Absent
    | Command of Command.t
    | Keyboard_macro of Key_sequence.t
    | Keymap of keymap
    | Symbol of Symbol.t
    | Undefined
    | Value of Value.t
  [@@deriving sexp_of]

  let to_value = function
    | Absent -> Value.nil
    | Command c -> c |> Command.to_value
    | Keyboard_macro k -> k |> Key_sequence.to_value
    | Keymap k -> k |> to_value
    | Symbol s -> s |> Symbol.to_value
    | Undefined -> Q.undefined |> Symbol.to_value
    | Value v -> v
  ;;

  let of_value_exn value =
    if Value.is_nil value
    then Absent
    else if Value.is_command value
    then Command (value |> Command.of_value_exn)
    else if Value.is_keymap value
    then Keymap (value |> of_value_exn)
    else if Value.eq value (Q.undefined |> Symbol.to_value)
    then Undefined
    else if Value.is_symbol value
    then Symbol (value |> Symbol.of_value_exn)
    else (
      match Key_sequence.of_value_exn value with
      | k -> Keyboard_macro k
      | exception _ -> Value value)
  ;;

  let type_ =
    Value.Type.create [%sexp "Keymap.Entry"] [%sexp_of: t] of_value_exn to_value
  ;;

  let t = type_
end

let lookup_key =
  Funcall.Wrap.("lookup-key" <: t @-> Key_sequence.t @-> bool @-> return value)
;;

let lookup_key_exn ?(accept_defaults = false) t key_sequence =
  let result = lookup_key t key_sequence accept_defaults in
  if Value.is_integer result
  then
    raise_s
      [%message
        "[Keymap.lookup_key_exn] got too long key sequence"
          (key_sequence : Key_sequence.t)];
  result |> Entry.of_value_exn
;;

let define_key =
  Funcall.Wrap.("define-key" <: t @-> Key_sequence.t @-> Entry.t @-> return nil)
;;

let minor_mode_map_alist =
  Var.Wrap.("minor-mode-map-alist" <: list (tuple Symbol.t type_))
;;

let minor_mode_overriding_map_alist =
  Buffer_local.Wrap.("minor-mode-overriding-map-alist" <: list (tuple Symbol.t type_))
;;

let find_minor_mode_map assoc symbol = List.Assoc.find assoc symbol ~equal:Symbol.equal

let override_minor_mode_map symbol ~f =
  match
    find_minor_mode_map
      (Buffer_local.Private.get_in_current_buffer minor_mode_overriding_map_alist)
      symbol
  with
  | Some t -> f t
  | None ->
    let t =
      match
        find_minor_mode_map (Current_buffer0.value_exn minor_mode_map_alist) symbol
      with
      | Some t -> deep_copy t
      | None -> create ()
    in
    f t;
    Buffer_local.Private.set_in_current_buffer
      minor_mode_overriding_map_alist
      ((symbol, t)
       :: Buffer_local.Private.get_in_current_buffer minor_mode_overriding_map_alist)
;;

let special_event_map = Var.Wrap.("special-event-map" <: t)
let suppress_keymap = Funcall.Wrap.("suppress-keymap" <: t @-> bool @-> return nil)
let suppress_keymap ?(suppress_digits = false) t = suppress_keymap t suppress_digits
