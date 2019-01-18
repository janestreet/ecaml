open! Core_kernel
open! Import0

module Q = struct
  include Q

  let default_boundp = "default-boundp" |> Symbol.intern
  and local_variable_if_set_p = "local-variable-if-set-p" |> Symbol.intern
  and make_variable_buffer_local = "make-variable-buffer-local" |> Symbol.intern
  and set_default = "set-default" |> Symbol.intern
end

type 'a t =
  { symbol : Symbol.t
  ; type_ : 'a Value.Type.t
  }
[@@deriving fields]

let sexp_of_t _ { symbol; type_ } =
  [%message "" ~_:(symbol : Symbol.t) ~_:(type_ : _ Value.Type.t)]
;;

type 'a var = 'a t [@@deriving sexp_of]

let create symbol type_ =
  { symbol
  ; type_ =
      Value.Type.with_of_value_exn type_ (fun value ->
        try Value.Type.of_value_exn type_ value with
        | exn ->
          raise_s
            [%message
              ""
                ~_:(concat [ "invalid value for variable: "; symbol |> Symbol.name ])
                ~_:(exn : exn)])
  }
;;

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

let is_buffer_local_if_set t buffer =
  Symbol.funcall2
    Q.local_variable_if_set_p
    (t.symbol |> Symbol.to_value)
    (buffer |> Buffer0.to_value)
  |> Value.to_bool
;;

let is_buffer_local_always var =
  let buffer = Buffer0.create ~name:"*for [Var.is_buffer_local_always]*" in
  let result = is_buffer_local_if_set var buffer in
  Buffer0.kill buffer;
  result
;;

module And_value = struct
  type t = T : 'a var * 'a -> t [@@deriving sexp_of]
end

module And_value_option = struct
  type t = T : 'a var * 'a option -> t [@@deriving sexp_of]
end
