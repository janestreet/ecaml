open! Core
open! Import0
module Current_buffer = Current_buffer0

include Value.Make_subtype (struct
    let name = "command"
    let here = [%here]
    let is_in_subtype = Value.is_command
  end)

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
        [%message "[Raw_prefix_argument.of_value] got unexpected value" (value : Value.t)]
  ;;

  let type_ =
    Value.Type.create [%message "raw_prefix_arg"] [%sexp_of: t] of_value_exn to_value
  ;;

  let t = type_

  let for_current_command =
    let current_prefix_arg = Var.Wrap.("current-prefix-arg" <: t) in
    fun () -> Current_buffer.value_exn current_prefix_arg
  ;;

  let numeric_value = Funcall.Wrap.("prefix-numeric-value" <: t @-> return int)
end

let inhibit_quit = Var.Wrap.("inhibit-quit" <: bool)
let quit_flag = Var.Wrap.("quit-flag" <: bool)
let request_quit () = Current_buffer.set_value quit_flag true
let suppress_quit () = Current_buffer.set_value quit_flag false

let abort_recursive_edit () =
  let abort_recursive_edit =
    Funcall.Wrap.("abort-recursive-edit" <: nullary @-> return nil)
  in
  abort_recursive_edit ();
  assert false
;;

module Private = struct
  let request_quit = request_quit
  let suppress_quit = suppress_quit
end
