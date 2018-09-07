open! Core_kernel
open! Import
include Defun_intf

module T0 = struct
  type _ t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Required : Symbol.t * 'a Value.Type.t -> 'a t
    | Optional : Symbol.t * 'a option Value.Type.t -> 'a option t
    | Rest : Symbol.t * 'a Value.Type.t -> 'a list t

  let return x = Return x
  let map t ~f = Map (t, f)
  let both t t' = Both (t, t')
  let apply f x = both f x |> map ~f:(fun (f, x) -> f x)
  let map = `Custom map
end

module T = struct
  include T0
  include Applicative.Make (T0)

  let required name type_ = Required (name, type_)
  let optional name type_ = Optional (name, Value.Type.option type_)
  let rest name type_ = Rest (name, type_)

  let optional_with_default name default type_ =
    Map (optional name type_, Option.value ~default)
  ;;

  include (Value.Type : Value.Type.S)
end

include T

module Open_on_rhs_intf = struct
  module type S = S with type 'a t = 'a t
end

include Applicative.Make_let_syntax (T) (Open_on_rhs_intf) (T)

let consume_arg args type_ ~pos =
  let arg = type_.Value.Type.of_value_exn args.(!pos) in
  Int.incr pos;
  arg
;;

let rec apply : type a. a t -> Value.t array -> len:int -> pos:int ref -> a =
  fun t args ~len ~pos ->
    match t with
    | Return a -> a
    | Map (t, f) -> f (apply t args ~len ~pos)
    | Both (t, t') ->
      let x = apply t args ~len ~pos in
      let x' = apply t' args ~len ~pos in
      x, x'
    | Required (_, type_) ->
      if Int.( >= ) !pos len
      then
        raise_s
          [%message
            "Not enough arguments.  Emacs should have raised wrong-number-of-arguments."]
      else consume_arg args type_ ~pos
    | Optional (_, type_) ->
      if Int.( >= ) !pos len then None else consume_arg args type_ ~pos
    | Rest (_, type_) ->
      let retval =
        Array.sub args ~pos:!pos ~len:(len - !pos)
        |> Array.map ~f:type_.of_value_exn
        |> Array.to_list
      in
      pos := len;
      retval
;;

class ['a] collect_names =
  object (self)
    method required _ acc = acc

    method optional _ acc = acc

    method rest _ acc = acc

    method go : 'a. 'a t -> Symbol.t list =
      fun t ->
        let rec aux : type a. a t -> Symbol.t list -> Symbol.t list =
          fun t acc ->
            match t with
            | Return _ -> acc
            | Map (t, _) -> aux t acc
            | Both (t, t') ->
              let acc = aux t acc in
              let acc = aux t' acc in
              acc
            | Required (name, _) -> self#required name acc
            | Optional (name, _) -> self#optional name acc
            | Rest (name, _) -> self#rest name acc
        in
        aux t [] |> List.rev
  end

let get_optional =
  object
    inherit ['a] collect_names

    method! optional = List.cons
  end
;;

let get_optional t =
  match get_optional#go t with
  | [] -> None
  | _ :: _ as names -> Some names
;;

let get_required =
  object
    inherit ['a] collect_names

    method! required = List.cons
  end
;;

let get_required t = get_required#go t

let get_rest =
  object
    inherit ['a] collect_names

    method! rest = List.cons
  end
;;

let get_rest t =
  match get_rest#go t with
  | [] -> None
  | [ name ] -> Some name
  | _ :: _ as names ->
    raise_s [%message "Multiple rest arguments" (names : Symbol.t list)]
;;

let get_fn t return_type args =
  let pos = ref 0 in
  let len = Array.length args in
  let retval = apply t args ~len ~pos in
  if Int.( = ) !pos len
  then return_type.Value.Type.to_value retval
  else
    raise_s
      [%message
        "Extra arguments.  Emacs should have raised wrong-number-of-arguments."
          ~used:(!pos : int)
          (args : Value.t array)]
;;

let defun
      ?(define_keys = [])
      ?docstring
      ?interactive
      ?obsoletes
      here
      return_type
      symbol
      t
  =
  Function.defun
    ?docstring
    ?interactive
    ?optional_args:(get_optional t)
    ?rest_arg:(get_rest t)
    here
    symbol
    (get_fn t return_type)
    ~args:(get_required t);
  List.iter define_keys ~f:(fun (keymap, keys) ->
    Keymap.define_key keymap (Key_sequence.create_exn keys) (Symbol symbol));
  Option.iter obsoletes ~f:(Obsolete.alias ~current:symbol)
;;

let defun_nullary
      ?define_keys
      ?docstring
      ?interactive
      ?obsoletes
      here
      return_type
      symbol
      f
  =
  defun
    ?define_keys
    ?docstring
    ?interactive
    ?obsoletes
    here
    return_type
    symbol
    (let open Let_syntax in
     let%map_open () = return () in
     f ())
;;

let defun_nullary_nil ?define_keys ?docstring ?interactive ?obsoletes here symbol f =
  defun_nullary
    ?define_keys
    ?docstring
    ?interactive
    ?obsoletes
    here
    Value.Type.unit
    symbol
    f
;;

let lambda ?docstring ?interactive here return_type t =
  Function.create
    ?docstring
    ?interactive
    ?optional_args:(get_optional t)
    ?rest_arg:(get_rest t)
    here
    ~args:(get_required t)
    (get_fn t return_type)
;;

let lambda_nullary ?docstring ?interactive here return_type f =
  lambda
    ?docstring
    ?interactive
    here
    return_type
    (let open Let_syntax in
     let%map_open () = return () in
     f ())
;;

let lambda_nullary_nil ?docstring ?interactive here f =
  lambda_nullary ?docstring ?interactive here Value.Type.unit f
;;
