open! Core_kernel
open! Import0

include Symbol0

module Q = struct
  let cl              = "cl"              |> intern
  let fboundp         = "fboundp"         |> intern
  let fset            = "fset"            |> intern
  let gensym          = "gensym"          |> intern
  let get             = "get"             |> intern
  let make_symbol     = "make-symbol"     |> intern
  let put             = "put"             |> intern
  let symbol_function = "symbol-function" |> intern
  let symbol_name     = "symbol-name"     |> intern
end

let name t = funcall1 Q.symbol_name (t |> to_value) |> Value.to_utf8_bytes_exn

let compare_name t1 t2 = String.compare (name t1) (name t2)

let function_is_defined t = funcall1 Q.fboundp (t |> to_value) |> Value.to_bool

let function_exn t =
  if not (function_is_defined t)
  then raise_s [%message
         "[Symbol.function_exn] of symbol with no function field"
           ~symbol:(t : t)];
  funcall1 Q.symbol_function (t |> to_value);
;;

let create ~name = funcall1 Q.make_symbol (name |> Value.of_utf8_bytes) |> of_value_exn

let require_cl = Memo.unit (fun () -> Feature0.require Q.cl)

let gensym ?prefix () =
  require_cl ();
  funcall1 Q.gensym (prefix |> Value.Type.(option string).to_value) |> of_value_exn
;;

let set_function t value = funcall2_i Q.fset (t |> to_value) value

type symbol = t [@@deriving sexp_of]

module Property = struct

  type 'a t =
    { name  : symbol
    ; type_ : 'a Value.Type.t }
  [@@deriving sexp_of]

  let create name type_ = { name; type_ }

  let get { name; type_ } sym =
    funcall2 Q.get
      (sym  |> to_value)
      (name |> to_value)
    |> (Value.Type.option type_).of_value_exn
  ;;

  let get_exn t symbol =
    match get t symbol with
    | Some value -> value
    | None -> raise_s [%message (symbol : symbol) "has no property" (t.name : symbol)]
  ;;

  let put { name; type_ } sym value =
    funcall3_i Q.put
      (sym   |> to_value)
      (name  |> to_value)
      (value |> type_.to_value)
  ;;

end

module type Subtype = sig
  type t

  val of_symbol_exn : symbol -> t
  val to_symbol     : t      -> symbol

  val of_value_exn : Value.t -> t
  val to_value     : t       -> Value.t
end

module Make_subtype (Arg : sig
    type t [@@deriving enumerate, sexp_of]
    val module_name : string
    val to_symbol : t -> symbol
  end) = struct

  let to_symbol = Arg.to_symbol

  let of_symbol_exn =
    let assoc = List.map Arg.all ~f:(fun arg -> (to_symbol arg, arg)) in
    fun symbol ->
      match List.Assoc.find assoc symbol ~equal with
      | Some t -> t
      | None ->
        raise_s [%message
          (concat [ "[";Arg.module_name;".of_symbol] got unexpected symbol" ])
            (symbol : t)]
  ;;

  let to_value t = t |> to_symbol |> to_value

  let of_value_exn value =
    match of_value_exn value with
    | s -> s |> of_symbol_exn
    | exception _ ->
      raise_s [%message
        (concat [ "[";Arg.module_name;".of_value_exn] got unexpected value" ])
          (value : Value.t)]
  ;;
end
