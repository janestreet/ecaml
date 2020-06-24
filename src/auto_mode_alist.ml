open! Core_kernel
open! Import

module Entry = struct
  type t =
    { delete_suffix_and_recur : bool
    ; filename_match : Regexp.t
    ; function_ : Symbol.t option
    }

  let sexp_of_t { delete_suffix_and_recur; filename_match; function_ } =
    [%message.omit_nil
      ""
        (filename_match : Regexp.t)
        (function_ : (Symbol.t option[@sexp.option]))
        (delete_suffix_and_recur : bool)]
  ;;

  let of_value_exn value =
    let filename_match = Value.car_exn value |> Regexp.of_value_exn in
    let value_to_function value =
      if Value.is_nil value then None else Some (value |> Symbol.of_value_exn)
    in
    let cdr = Value.cdr_exn value in
    if Value.is_symbol cdr
    then
      { delete_suffix_and_recur = false
      ; filename_match
      ; function_ = cdr |> value_to_function
      }
    else
      { delete_suffix_and_recur = Value.car_exn (Value.cdr_exn cdr) |> Value.to_bool
      ; filename_match
      ; function_ = Value.car_exn cdr |> value_to_function
      }
  ;;

  let to_value t =
    let function_ =
      match t.function_ with
      | None -> Value.nil
      | Some function_ -> function_ |> Symbol.to_value
    in
    let filename_match = t.filename_match |> Regexp.to_value in
    if t.delete_suffix_and_recur
    then Value.list [ filename_match; function_; Value.t ]
    else Value.cons filename_match function_
  ;;

  let type_ =
    Value.Type.create
      [%message "Auto_mode_alist.Entry"]
      [%sexp_of: t]
      of_value_exn
      to_value
  ;;

  let t = type_
end

type t = Entry.t list [@@deriving sexp_of]

let type_ = Value.Type.list Entry.t
let t = type_
let auto_mode_alist = Var.Wrap.("auto-mode-alist" <: t)
let auto_mode_alist_value = Var.Wrap.("auto-mode-alist" <: value)
let append = Funcall.Wrap.("append" <: t @-> value @-> return value)

let add entries =
  Current_buffer.set_value
    auto_mode_alist_value
    (append entries (Current_buffer.value_exn auto_mode_alist_value))
;;
