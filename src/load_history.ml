open! Core_kernel
open! Import

type t = Value.t
[@@deriving sexp_of]

let q = Q.load_history

let get () = Symbol.value_exn q

let set v = Symbol.set_value q v

let defining_file symbol =
  let result = Symbol.funcall1 Q.symbol_file (symbol |> Symbol.to_value) in
  if Value.is_nil result
  then None
  else Some (result |> Value.to_utf8_bytes_exn)
;;

module Entry = struct
  type t =
    | Autoload               of Symbol.t
    | Face                   of Face.t
    | Fun                    of Symbol.t
    | Previously_an_autoload of Symbol.t
    | Provide                of Symbol.t
    | Require                of Symbol.t
    | Var                    of Symbol.t
  [@@deriving sexp_of]

  let cons s v = Value.cons (s |> Symbol.to_value) v

  let to_value = function
    | Autoload               s -> cons Q.autoload (s |> Symbol.to_value)
    | Face                   f -> cons Q.defface  (f |> Face.to_value)
    | Fun                    s -> cons Q.defun    (s |> Symbol.to_value)
    | Previously_an_autoload s -> cons Q.t        (s |> Symbol.to_value)
    | Provide                s -> cons Q.provide  (s |> Symbol.to_value)
    | Require                s -> cons Q.require  (s |> Symbol.to_value)
    | Var                    s -> s |> Symbol.to_value
  ;;
end

let add_defuns ~chop_prefix ~in_dir =
  let entries =
    Function.get_and_clear_defuns ()
    |> List.map ~f:(fun ((source_code_position : Source_code_position.t), symbol) ->
      (String.chop_prefix_exn source_code_position.pos_fname ~prefix:chop_prefix
      , symbol))
    |> String.Table.of_alist_multi
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (file, symbols) ->
      Value.cons
        (Caml.Filename.concat in_dir file |> Value.of_utf8_bytes)
        (Value.list (symbols
                     |> List.map ~f:(fun symbol -> Entry.Fun symbol |> Entry.to_value))))
    |> Value.list in
  set (Symbol.funcall2 Q.append entries (get ()));
;;
