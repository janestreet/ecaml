open! Core_kernel
open! Import0
open! Async_kernel
module Buffer = Buffer0

let get = Funcall.Wrap.("current-buffer" <: nullary @-> return Buffer.t)
let set = Funcall.Wrap.("set-buffer" <: Buffer.t @-> return nil)

let set_temporarily sync_or_async buffer ~f =
  let old = get () in
  set buffer;
  Sync_or_async.protect [%here] sync_or_async ~f ~finally:(fun () ->
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

let set_value (var : _ Var.t) a =
  elisp_set var.symbol (a |> Value.Type.to_value var.type_)
;;

let set_values vars_and_values =
  List.iter vars_and_values ~f:(fun (Var.And_value.T (var, value)) ->
    set_value var value)
;;

let set_values_temporarily sync_or_async vars_and_values ~f =
  let old_buffer = get () in
  let olds =
    List.map vars_and_values ~f:(fun (Var.And_value.T (var, _)) ->
      Var.And_value_option.T (var, value var))
  in
  List.iter vars_and_values ~f:(fun (Var.And_value.T (var, value)) ->
    set_value var value);
  Sync_or_async.protect [%here] sync_or_async ~f ~finally:(fun () ->
    let new_buffer = get () in
    let buffer_changed = not (Buffer.equal old_buffer new_buffer) in
    if buffer_changed then set old_buffer;
    List.iter olds ~f:(fun (Var.And_value_option.T (var, value_opt)) ->
      match value_opt with
      | None -> clear_value var
      | Some value -> set_value var value);
    if buffer_changed then set new_buffer)
;;

let set_value_temporarily sync_or_async var value ~f =
  set_values_temporarily sync_or_async [ T (var, value) ] ~f
;;

let has_non_null_value var =
  match value { var with type_ = Value.Type.bool } with
  | None -> false
  | Some b -> b
;;
