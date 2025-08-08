open! Core
open! Import0
open! Async_kernel
module Buffer = Buffer0

module Q = struct
  let funcall = "funcall" |> Symbol.intern
end

let get = Funcall.Wrap.("current-buffer" <: nullary @-> return Buffer.t)
let set = Funcall.Wrap.("set-buffer" <: Buffer.t @-> return nil)

let set_temporarily ?(here = Stdlib.Lexing.dummy_pos) sync_or_async buffer ~f =
  let old = get () in
  set buffer;
  Sync_or_async.protect ~here sync_or_async ~f ~finally:(fun () ->
    if Buffer.is_live old then set old)
;;

let boundp = Funcall.Wrap.("boundp" <: Symbol.t @-> return bool)
let variable_is_defined = boundp
let value_is_defined (var : _ Var.t) = boundp var.symbol
let symbol_value = Funcall.Wrap.("symbol-value" <: Symbol.t @-> return value)

let value_internal (var : _ Var.t) =
  symbol_value var.symbol |> Value.Type.of_value_exn var.type_
;;

let value var = if not (value_is_defined var) then None else Some (value_internal var)

let value_exn var =
  if not (value_is_defined var)
  then
    raise_s
      [%message "[Current_buffer.value_exn] of undefined variable" ~_:(var : _ Var.t)];
  value_internal var
;;

let value_opt_exn var =
  match value_exn var with
  | Some x -> x
  | None ->
    raise_s
      [%message
        "buffer unexpectedly does not give a value to variable"
          (var : _ Var.t)
          ~buffer:(get () : Buffer.t)]
;;

let makunbound = Funcall.Wrap.("makunbound" <: Symbol.t @-> return nil)
let clear_value (var : _ Var.t) = makunbound var.symbol
let elisp_set = Funcall.Wrap.("set" <: Symbol.t @-> value @-> return nil)
let set_value (var : _ Var.t) a = elisp_set var.symbol (a |> Value.Type.to_value var.type_)

let set_values vars_and_values =
  List.iter vars_and_values ~f:(fun (Var.And_value.T (var, value)) -> set_value var value)
;;

let set_values_temporarily
  (type a b)
  (sync_or_async : (a, b) Sync_or_async.t)
  (vars_and_values : Var.And_value.t list)
  ~(f : unit -> b)
  =
  let r : a Set_once.t = Set_once.create () in
  let f =
    match sync_or_async with
    | Sync ->
      Function.of_ocaml_func0 [%here] (fun () ->
        Set_once.set_exn r (f ());
        Value.nil)
    | Async ->
      Function.of_ocaml_func0 [%here] (fun () ->
        Set_once.set_exn r (Value.Private.block_on_async f);
        Value.nil)
  in
  let form =
    Form.let_
      (List.map vars_and_values ~f:(fun (Var.And_value.T (var, value)) ->
         var.symbol, Form.quote (Value.Type.to_value var.type_ value)))
      (Form.apply Q.funcall [ Form.quote (Function.to_value f) ])
  in
  match sync_or_async with
  | Sync ->
    Form.Blocking.eval_i form;
    (Set_once.get_exn r : b)
  | Async ->
    let%bind () = Form.eval_i form in
    return (Set_once.get_exn r)
;;

let set_value_temporarily sync_or_async var value ~f =
  set_values_temporarily sync_or_async [ T (var, value) ] ~f
;;

let has_non_null_value var =
  match value { var with type_ = Value.Type.bool } with
  | None -> false
  | Some b -> b
;;

let rename_buffer = Funcall.Wrap.("rename-buffer" <: string @-> bool @-> return nil)
let rename_exn ?(unique = false) () ~name = rename_buffer name unique
