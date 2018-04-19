open! Core_kernel
open! Import

module Q = struct
  include Q
  let copy_keymap                      = "copy-keymap"                      |> Symbol.intern
  let current_global_map               = "current-global-map"               |> Symbol.intern
  let define_key                       = "define-key"                       |> Symbol.intern
  let keymap_parent                    = "keymap-parent"                    |> Symbol.intern
  let lookup_key                       = "lookup-key"                       |> Symbol.intern
  let make_keymap                      = "make-keymap"                      |> Symbol.intern
  let make_sparse_keymap               = "make-sparse-keymap"               |> Symbol.intern
  let set_keymap_parent                = "set-keymap-parent"                |> Symbol.intern
  let set_transient_map                = "set-transient-map"                |> Symbol.intern
  let undefined                        = "undefined"                        |> Symbol.intern
  let use_global_map                   = "use-global-map"                   |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "keymap"
    let here = [%here]
    let is_in_subtype = Value.is_keymap
  end)

type keymap = t [@@deriving sexp_of]

let equal = eq

let parent t =
  let result = Symbol.funcall1 Q.keymap_parent (t |> to_value) in
  if Value.is_nil result
  then None
  else Some (result |> of_value_exn)
;;

let set_parent t parent =
  Symbol.funcall2_i Q.set_keymap_parent (t |> to_value)
    (match parent with
     | None -> Value.nil
     | Some parent -> parent |> to_value);
;;

let set_transient t = Symbol.funcall1_i Q.set_transient_map (t |> to_value)

module Kind = struct
  type t =
    | Full
    | Sparse
  [@@deriving sexp_of]
end

let create ?(kind = Kind.Sparse) ?menu_name () =
  Symbol.funcall1
    (match kind with
     | Full -> Q.make_keymap
     | Sparse -> Q.make_sparse_keymap)
    (match menu_name with
     | None -> Value.nil
     | Some menu_name -> menu_name |> Value.of_utf8_bytes)
  |> of_value_exn
;;

let deep_copy t = Symbol.funcall1 Q.copy_keymap (t |> to_value) |> of_value_exn

let global () = Symbol.funcall0 Q.current_global_map |> of_value_exn

let set_global t = Symbol.funcall1_i Q.use_global_map (t |> to_value)

module Entry = struct
  type t =
    | Absent
    | Command        of Command.t
    | Keyboard_macro of Key_sequence.t
    | Keymap         of keymap
    | Symbol         of Symbol.t
    | Undefined
    | Value          of Value.t
  [@@deriving sexp_of]

  let to_value = function
    | Absent           -> Value.nil
    | Command c        -> c |> Command.to_value
    | Keyboard_macro k -> k |> Key_sequence.to_value
    | Keymap k         -> k |> to_value
    | Symbol s         -> s |> Symbol.to_value
    | Undefined        -> Q.undefined |> Symbol.to_value
    | Value v          -> v
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

  let type_ = { Value.Type.
                name = [%sexp "Keymap.Entry"]
              ; to_value
              ; of_value_exn }
end

let lookup_key_exn ?(accept_defaults = false) t key_sequence =
  let result =
    Symbol.funcall3 Q.lookup_key
      (t |> to_value)
      (key_sequence |> Key_sequence.to_value)
      (accept_defaults |> Value.of_bool) in
  if Value.is_integer result
  then raise_s [%message
         "[Keymap.lookup_key_exn] got too long key sequence"
           (key_sequence : Key_sequence.t)];
  result |> Entry.of_value_exn
;;

let define_key t key_sequence entry =
  Symbol.funcall3_i Q.define_key
    (t |> to_value)
    (key_sequence |> Key_sequence.to_value)
    (entry |> Entry.to_value)
;;
