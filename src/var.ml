open! Core_kernel
open! Import0

module Q = struct
  include Q
  let default_boundp                   = "default-boundp"                   |> Symbol.intern
  let make_variable_buffer_local       = "make-variable-buffer-local"       |> Symbol.intern
  let set_default                      = "set-default"                      |> Symbol.intern
end

type 'a t =
  { symbol : Symbol.t
  ; type_  : 'a Value.Type.t }
[@@deriving fields]

let sexp_of_t _ { symbol; type_ } =
  [%message "" ~_:(symbol : Symbol.t) ~_:(type_ : _ Value.Type.t)]
;;

type 'a var = 'a t [@@deriving sexp_of]

let create symbol type_ = { symbol; type_ }

let symbol_as_value t = t.symbol |> Symbol.to_value

let default_value_exn t =
  Symbol.funcall1 Q.default_value (symbol_as_value t) |> t.type_.of_value_exn
;;

let default_value_is_defined t =
  Symbol.funcall1 Q.default_boundp (symbol_as_value t) |> Value.to_bool
;;

let set_default_value t a =
  Symbol.funcall2_i Q.set_default (symbol_as_value t) (a |> t.type_.to_value)
;;

let make_buffer_local_always t =
  let symbol = symbol_as_value t in
  add_gc_root symbol;
  Symbol.funcall1_i Q.make_variable_buffer_local symbol
;;

module And_value = struct
  type t = T : 'a var * 'a -> t
  [@@deriving sexp_of]
end

module And_value_option = struct
  type t = T : 'a var * 'a option -> t
  [@@deriving sexp_of]
end
