open! Core
open! Import0
include Ecaml_value.Symbol

let name = Funcall.Wrap.("symbol-name" <: t @-> return string)
let compare_name t1 t2 = String.compare (name t1) (name t2)
let function_is_defined = Funcall.Wrap.("fboundp" <: t @-> return bool)
let symbol_function = Funcall.Wrap.("symbol-function" <: t @-> return value)

let function_exn t =
  if not (function_is_defined t)
  then
    raise_s
      [%message "[Symbol.function_exn] of symbol with no function field" ~symbol:(t : t)];
  symbol_function t
;;

let gensym = Funcall.Wrap.("gensym" <: nil_or string @-> return t)
let gensym ?prefix () = gensym prefix

module Automatic_migration = struct
  module New = struct
    type nonrec t =
      { new_ : t
      ; since : string
      }
    [@@deriving sexp_of]
  end

  type one = old:t -> New.t option

  let all = ref []
  let add (one : one) = all := !all @ [ one ]
  let migrate ~old = List.find_map !all ~f:(fun f -> f ~old)
end

type symbol = t [@@deriving sexp_of]

module Property = struct
  type 'a t =
    { name : symbol
    ; type_ : 'a Value.Type.t
    }
  [@@deriving sexp_of]

  let create name type_ = { name; type_ }
  let get = Funcall.Wrap.("get" <: t @-> t @-> return value)
  let get { name; type_ } sym = get sym name |> Value.Type.(nil_or type_ |> of_value_exn)

  let get_exn t symbol =
    match get t symbol with
    | Some value -> value
    | None -> raise_s [%message (symbol : symbol) "has no property" (t.name : symbol)]
  ;;

  let put = Funcall.Wrap.("put" <: t @-> t @-> value @-> return nil)
  let put { name; type_ } sym value = put sym name (value |> Value.Type.to_value type_)

  let function_documentation =
    create ("function-documentation" |> intern) Value.Type.value
  ;;

  let variable_documentation =
    create ("variable-documentation" |> intern) Value.Type.value
  ;;

  let advertised_binding = create (":advertised-binding" |> intern) Key_sequence0.type_
end

module type Subtype = sig
  type t

  val of_symbol_exn : symbol -> t
  val to_symbol : t -> symbol
  val of_value_exn : Value.t -> t
  val to_value : t -> Value.t
end

module Make_subtype (Arg : sig
    type t [@@deriving enumerate, sexp_of]

    val module_name : string
    val to_symbol : t -> symbol
  end) =
struct
  let to_symbol = Arg.to_symbol

  let of_symbol_exn =
    let assoc = List.map Arg.all ~f:(fun arg -> to_symbol arg, arg) in
    fun symbol ->
      match List.Assoc.find assoc symbol ~equal with
      | Some t -> t
      | None ->
        raise_s
          [%message
            [%string "[%{Arg.module_name}.of_symbol] got unexpected symbol"] (symbol : t)]
  ;;

  let to_value t = t |> to_symbol |> to_value

  let of_value_exn value =
    match of_value_exn value with
    | s -> s |> of_symbol_exn
    | exception _ ->
      raise_s
        [%message
          [%string "[%{Arg.module_name}.of_value_exn] got unexpected value"]
            (value : Value.t)]
  ;;
end

module Compare_name = struct
  module T = struct
    type t = symbol [@@deriving sexp_of]

    let compare = compare_name
  end

  include T
  include Comparator.Make (T)
end
