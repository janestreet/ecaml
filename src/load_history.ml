open! Core_kernel
open! Import0
module Face = Face0

module Q = struct
  include Q

  let autoload = "autoload" |> Symbol.intern
  let provide = "provide" |> Symbol.intern
  let require = "require" |> Symbol.intern
end

module Current_buffer = Current_buffer0

type t = Value.t [@@deriving sexp_of]

let load_history = Var.Wrap.("load-history" <: value)
let defining_file = Funcall.Wrap.("symbol-file" <: Symbol.t @-> return (nil_or string))

module Entry = struct
  type t =
    | Autoload of Symbol.t
    | Face of Face.t
    | Fun of Symbol.t
    | Previously_an_autoload of Symbol.t
    | Provide of Symbol.t
    | Require of Symbol.t
    | Var of Symbol.t
  [@@deriving sexp_of]

  let cons s v = Value.cons (s |> Symbol.to_value) v

  let to_value = function
    | Autoload s -> cons Q.autoload (s |> Symbol.to_value)
    | Face f -> cons Q.defface (f |> Face.to_value)
    | Fun s -> cons Q.defun (s |> Symbol.to_value)
    | Previously_an_autoload s -> cons Q.t (s |> Symbol.to_value)
    | Provide s -> cons Q.provide (s |> Symbol.to_value)
    | Require s -> cons Q.require (s |> Symbol.to_value)
    | Var s -> s |> Symbol.to_value
  ;;
end

module Type = struct
  module T = struct
    type t =
      | Face
      | Fun
      | Var
    [@@deriving compare, enumerate, hash, sexp_of]
  end

  include T

  include Valueable.Remove_t
      ((val Valueable.of_type
              (Value.Type.enum
                 [%sexp "load-history type"]
                 (module T)
                 (function
                   | Face -> Q.defface |> Symbol.to_value
                   | Fun -> Value.nil
                   | Var -> Q.defvar |> Symbol.to_value))))
end

module Key = struct
  module T = struct
    type t =
      { symbol_name : string
      ; type_ : Type.t
      }
    [@@deriving compare, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)

  let create symbol type_ = { symbol_name = Symbol.name symbol; type_ }
end

let location_by_key : Source_code_position.t Key.Table.t = Key.Table.create ()

let location_exn symbol type_ =
  match Hashtbl.find location_by_key (Key.create symbol type_) with
  | Some x -> x
  | None ->
    raise_s
      [%message "don't know location of symbol" (symbol : Symbol.t) (type_ : Type.t)]
;;

let entries = ref []

let add_entry here (entry : Entry.t) =
  entries := (here, entry) :: !entries;
  let add symbol type_ =
    Hashtbl.set location_by_key ~key:(Key.create symbol type_) ~data:here
  in
  match entry with
  | Face face -> add (face |> Face.to_name |> Symbol.intern) Face
  | Fun symbol -> add symbol Fun
  | Var symbol -> add symbol Var
  | _ -> ()
;;

let append = Funcall.Wrap.("append" <: value @-> value @-> return value)

let update_emacs_with_entries ~chop_prefix ~in_dir =
  let addition =
    !entries
    |> List.map ~f:(fun ((source_code_position : Source_code_position.t), entry) ->
      ( String.chop_prefix_exn source_code_position.pos_fname ~prefix:chop_prefix
      , entry ))
    |> String.Table.of_alist_multi
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (file, entries) ->
      Value.cons
        (Caml.Filename.concat in_dir file |> Value.of_utf8_bytes)
        (Value.list (entries |> List.map ~f:Entry.to_value)))
    |> Value.list
  in
  entries := [];
  Current_buffer.set_value
    load_history
    (append addition (Current_buffer.value_exn load_history))
;;
