open! Core_kernel
open! Import0

module Q = struct
  include Q

  let boundp = "boundp" |> Symbol.intern
  and current_buffer = "current-buffer" |> Symbol.intern
  and makunbound = "makunbound" |> Symbol.intern
  and set_buffer = "set-buffer" |> Symbol.intern
  and symbol_value = "symbol-value" |> Symbol.intern
end

module Buffer = Buffer0

let get () = Symbol.funcall0 Q.current_buffer |> Buffer.of_value_exn
let set t = Symbol.funcall1_i Q.set_buffer (t |> Buffer.to_value)

let set_temporarily t ~f =
  let old = get () in
  set t;
  protect ~f ~finally:(fun () -> set old)
;;

let value_is_defined (var : _ Var.t) =
  Symbol.funcall1 Q.boundp (var.symbol |> Symbol.to_value) |> Value.to_bool
;;

let symbol_value symbol = Symbol.funcall1 Q.symbol_value (symbol |> Symbol.to_value)
let value_internal (var : _ Var.t) = symbol_value var.symbol |> var.type_.of_value_exn
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

let clear_value (var : _ Var.t) =
  Symbol.funcall1_i Q.makunbound (var.symbol |> Symbol.to_value)
;;

let set_value (var : _ Var.t) a =
  Symbol.funcall2_i Q.set (var.symbol |> Symbol.to_value) (a |> var.type_.to_value)
;;

let set_values vars_and_values =
  List.iter vars_and_values ~f:(fun (Var.And_value.T (var, value)) -> set_value var value)
;;

let set_values_temporarily vars_and_values ~f =
  let old_buffer = get () in
  let olds =
    List.map vars_and_values ~f:(fun (Var.And_value.T (var, _)) ->
      Var.And_value_option.T (var, value var))
  in
  List.iter vars_and_values ~f:(fun (Var.And_value.T (var, value)) -> set_value var value);
  protect ~f ~finally:(fun () ->
    let new_buffer = get () in
    let buffer_changed = not (Buffer.equal old_buffer new_buffer) in
    if buffer_changed then set old_buffer;
    List.iter olds ~f:(fun (Var.And_value_option.T (var, value_opt)) ->
      match value_opt with
      | None -> clear_value var
      | Some value -> set_value var value);
    if buffer_changed then set new_buffer)
;;

let set_value_temporarily var value ~f = set_values_temporarily [ T (var, value) ] ~f

let has_non_null_value var =
  match value { var with type_ = Value.Type.bool } with
  | None -> false
  | Some b -> b
;;
