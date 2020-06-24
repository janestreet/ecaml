open! Core_kernel
open! Import0
module Current_buffer = Current_buffer0

include Value.Make_subtype (struct
    let name = "command"
    let here = [%here]
    let is_in_subtype = Value.is_command
  end)

let history_var = Var.Wrap.("command-history" <: list Form.t)
let history () = Current_buffer0.value_exn history_var

module Raw_prefix_argument = struct
  type t =
    | Absent
    | Int of int
    | Minus
    | Nested of int
  [@@deriving sexp_of]

  let minus = "-" |> Value.intern

  let to_value = function
    | Absent -> Value.nil
    | Int i -> i |> Value.of_int_exn
    | Minus -> minus
    | Nested i -> Value.cons (i |> Value.of_int_exn) Value.nil
  ;;

  let of_value_exn value =
    if Value.is_nil value
    then Absent
    else if Value.is_integer value
    then Int (Value.to_int_exn value)
    else if Value.is_cons value
    then Nested (Value.car_exn value |> Value.to_int_exn)
    else if Value.eq value minus
    then Minus
    else
      raise_s
        [%message
          "[Raw_prefix_argument.of_value] got unexpected value" (value : Value.t)]
  ;;

  let type_ =
    Value.Type.create [%message "raw_prefix_arg"] [%sexp_of: t] of_value_exn to_value
  ;;

  let t = type_
  let for_current_command = Var.Wrap.("current-prefix-arg" <: t)
  let numeric_value = Funcall.Wrap.("prefix-numeric-value" <: t @-> return int)
end

let call_interactively =
  let call_interactively =
    Funcall.Wrap.("call-interactively" <: value @-> bool @-> return nil)
  in
  fun ?(raw_prefix_argument = Raw_prefix_argument.Absent) ?(record = false) command ->
    Value.Private.run_outside_async [%here] (fun () ->
      Current_buffer.set_value
        Raw_prefix_argument.for_current_command
        raw_prefix_argument;
      call_interactively command record)
;;

let inhibit_quit = Var.Wrap.("inhibit-quit" <: bool)
let quit_flag = Var.Wrap.("quit-flag" <: bool)
let request_quit () = Current_buffer.set_value quit_flag true

let quit_requested () =
  (* We use [try-with] because calling into Elisp can itself check [quit-flag]
     and raise.  And in fact does, at least in Emacs 25.2. *)
  try Current_buffer.value_exn quit_flag with
  | _ -> true
;;

let abort_recursive_edit () =
  let abort_recursive_edit =
    Funcall.Wrap.("abort-recursive-edit" <: nullary @-> return nil)
  in
  abort_recursive_edit ();
  assert false
;;
