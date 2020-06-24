open! Core_kernel
open! Import0
include Var_intf
module Buffer = Buffer0

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

module Wrap = struct
  let ( <: ) name type_ = create (name |> Symbol.intern) type_

  include (Value.Type : Value.Type.S)
end

let symbol_as_value t = t.symbol |> Symbol.to_value
let default_value = Funcall.Wrap.("default-value" <: Symbol.t @-> return value)
let default_value_exn t = default_value t.symbol |> Value.Type.of_value_exn t.type_
let default_boundp = Funcall.Wrap.("default-boundp" <: Symbol.t @-> return bool)
let default_value_is_defined t = default_boundp t.symbol
let set_default = Funcall.Wrap.("set-default" <: Symbol.t @-> value @-> return nil)
let set_default_value t a = set_default t.symbol (a |> Value.Type.to_value t.type_)

let make_variable_buffer_local =
  Funcall.Wrap.("make-variable-buffer-local" <: Symbol.t @-> return nil)
;;

let make_buffer_local_always t =
  add_gc_root (symbol_as_value t);
  make_variable_buffer_local t.symbol
;;

let local_variable_if_set_p =
  Funcall.Wrap.("local-variable-if-set-p" <: Symbol.t @-> Buffer.t @-> return bool)
;;

let is_buffer_local_if_set t buffer = local_variable_if_set_p t.symbol buffer

let is_buffer_local_always var =
  let buffer = Buffer.create ~name:"*for [Var.is_buffer_local_always]*" in
  let result = is_buffer_local_if_set var buffer in
  Buffer.Blocking.kill buffer;
  result
;;

module And_value = struct
  type t = T : 'a var * 'a -> t [@@deriving sexp_of]
end

module And_value_option = struct
  type t = T : 'a var * 'a option -> t [@@deriving sexp_of]
end
