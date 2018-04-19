open! Core_kernel
open! Import

module Q = struct
  include Q
  let call_interactively               = "call-interactively"               |> Symbol.intern
  let current_prefix_arg               = "current-prefix-arg"               |> Symbol.intern
  let prefix_numeric_value             = "prefix-numeric-value"             |> Symbol.intern
end

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
    else raise_s [%message
           "[Raw_prefix_argument.of_value] got unexpected value" (value : Value.t)]
  ;;

  let type_ =
    { Value.Type.
      name = [%message "raw_prefix_arg"]
    ; of_value_exn
    ; to_value }
  ;;

  let for_current_command = Var.create Q.current_prefix_arg type_

  let numeric_value t =
    Symbol.funcall1 Q.prefix_numeric_value (t |> to_value) |> Value.to_int_exn
  ;;
end

let call_interactively value raw_prefix_argument =
  Current_buffer.set_value Raw_prefix_argument.for_current_command raw_prefix_argument;
  Symbol.funcall1_i Q.call_interactively value;
;;
