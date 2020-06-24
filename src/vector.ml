open! Core_kernel
open! Import

module Q = struct
  include Q

  let vconcat = "vconcat" |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "vector"
    let here = [%here]
    let is_in_subtype = Value.is_vector
  end)

let make_vector = Funcall.Wrap.("make-vector" <: int @-> value @-> return t)
let create ~len value = make_vector len value
let length = Funcall.Wrap.("length" <: t @-> return int)

let bounds_check t i name =
  let length = length t in
  if i < 0 || i >= length
  then
    raise_s
      [%message
        (concat [ "[Vector."; name; "] got invalid subscript" ])
          ~subscript:(i : int)
          (length : int)
          ~vector:(t : t)]
;;

let aref = Funcall.Wrap.("aref" <: t @-> int @-> return value)

let get t i =
  bounds_check t i "get";
  aref t i
;;

let aset = Funcall.Wrap.("aset" <: t @-> int @-> value @-> return nil)

let set t i v =
  bounds_check t i "set";
  aset t i v
;;

let of_list vs = Symbol.funcallN Q.vector vs |> of_value_exn
let concat ts = Symbol.funcallN Q.vconcat (ts : t list :> Value.t list) |> of_value_exn
let to_array t ~f = Array.init (length t) ~f:(fun i -> get t i |> f)

let type_ (type a) (type_ : a Value.Type.t) =
  Value.Type.create
    [%message "vector" ~_:(type_ : _ Value.Type.t)]
    (sexp_of_array (Value.Type.to_sexp type_))
    (fun v -> v |> of_value_exn |> to_array ~f:(Value.Type.of_value_exn type_))
    (fun a ->
       a
       |> Array.map ~f:(Value.Type.to_value type_)
       |> Array.to_list
       |> of_list
       |> to_value)
;;

let t = type_
