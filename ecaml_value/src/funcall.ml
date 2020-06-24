open! Core_kernel
open! Import

type _ t =
  | Cons : 'a Value.Type.t * 'b t -> ('a -> 'b) t
  | Nullary : 'b Value.Type.t -> (unit -> 'b) t
  | Return : 'a Value.Type.t -> 'a t

let return_type_of_value symbol (type_ : 'a Value.Type.t) value =
  match Value.Type.of_value_exn type_ value with
  | x -> x
  | exception exn ->
    raise_s
      [%message
        "funcall failed to convert return value."
          (symbol : Value.t)
          (type_ : _ Value.Type.t)
          (exn : Exn.t)]
;;

let arity t =
  let rec arity : type a. a t -> int -> int =
    fun t i ->
      match t with
      | Return _ -> i
      | Nullary _ -> i
      | Cons (_, t) -> arity t (i + 1)
  in
  arity t 0
;;

let wrap : type a. a t -> Value.t -> a =
  fun t symbol ->
  let rec curry : type a. a t -> Value.t -> Value.t array -> int -> a =
    fun t symbol args i ->
      match t with
      | Cons (type_, t) ->
        fun arg ->
          args.(i) <- Value.Type.to_value type_ arg;
          curry t symbol args (i + 1)
      | Nullary return_type ->
        assert (Int.( = ) i 0);
        fun _ -> Value.funcall0 symbol |> return_type_of_value symbol return_type
      | Return type_ ->
        Value.funcallN_array symbol args |> return_type_of_value symbol type_
  in
  let args = Array.create ~len:(arity t) Value.nil in
  curry t symbol args 0
;;

(* It's unclear how much this sort of unrolling matters, but the C bindings do it, so we
   might as well do it here. *)
let wrap_unrolled : type a. a t -> Value.t -> a =
  fun t symbol ->
  let ret type_ value = return_type_of_value symbol type_ value in
  match t with
  | Return type_ -> Value.funcall0 symbol |> ret type_
  | Nullary return_type -> fun _ -> Value.funcall0 symbol |> ret return_type
  | Cons (type1, Return type_) ->
    fun a1 -> Value.funcall1 symbol (a1 |> Value.Type.to_value type1) |> ret type_
  | Cons (type1, Cons (type2, Return type_)) ->
    fun a1 a2 ->
      Value.funcall2
        symbol
        (a1 |> Value.Type.to_value type1)
        (a2 |> Value.Type.to_value type2)
      |> ret type_
  | Cons (type1, Cons (type2, Cons (type3, Return type_))) ->
    fun a1 a2 a3 ->
      Value.funcall3
        symbol
        (a1 |> Value.Type.to_value type1)
        (a2 |> Value.Type.to_value type2)
        (a3 |> Value.Type.to_value type3)
      |> ret type_
  | Cons (type1, Cons (type2, Cons (type3, Cons (type4, Return type_)))) ->
    fun a1 a2 a3 a4 ->
      Value.funcall4
        symbol
        (a1 |> Value.Type.to_value type1)
        (a2 |> Value.Type.to_value type2)
        (a3 |> Value.Type.to_value type3)
        (a4 |> Value.Type.to_value type4)
      |> ret type_
  | Cons (type1, Cons (type2, Cons (type3, Cons (type4, Cons (type5, Return type_))))) ->
    fun a1 a2 a3 a4 a5 ->
      Value.funcall5
        symbol
        (a1 |> Value.Type.to_value type1)
        (a2 |> Value.Type.to_value type2)
        (a3 |> Value.Type.to_value type3)
        (a4 |> Value.Type.to_value type4)
        (a5 |> Value.Type.to_value type5)
      |> ret type_
  | t -> wrap t symbol
;;

let apply t f args ~on_parse_error =
  let wrong_number_of_args message =
    raise_s [%message message (arity t : int) (args : Value.t list)]
  in
  let rec apply : type a. a t -> a -> Value.t list -> Value.t =
    fun t f args ->
      match t with
      | Cons (type_, t) ->
        (match args with
         | arg :: args ->
           (match Value.Type.of_value_exn type_ arg with
            | arg -> apply t (f arg) args
            | exception exn -> on_parse_error exn)
         | [] ->
           (* Emacs convention: missing arguments are nil. *)
           (match Value.Type.of_value_exn type_ Value.nil with
            | arg -> apply t (f arg) args
            | exception exn -> on_parse_error exn))
      | Return type_ ->
        (match args with
         | [] -> Value.Type.to_value type_ f
         | _ :: _ -> wrong_number_of_args "Extra args.")
      | Nullary type_ ->
        (match args with
         | [] -> Value.Type.to_value type_ (f ())
         | _ :: _ -> wrong_number_of_args "Extra args.")
  in
  apply t f args
;;

module Wrap = struct
  let return type_ = Return type_

  let nullary =
    Value.Type.create
      [%sexp "funcall-nullary-placeholder-value"]
      [%sexp_of: unit]
      ignore
      (const Value.nil)
  ;;

  let nil = Value.Type.ignored

  let ( @-> ) (type a b) (type_ : a Value.Type.t) (t : b t) =
    match t with
    | Cons _ ->
      (match Type_equal.Id.same (Value.Type.id type_) (Value.Type.id nullary) with
       | true -> raise_s [%message "Function already has arguments, cannot be nullary."]
       | false -> Cons (type_, t))
    | Nullary _ -> raise_s [%message "Cannot add arguments to nullary function."]
    | Return return_type ->
      (match
         Type_equal.Id.same_witness (Value.Type.id type_) (Value.Type.id nullary)
       with
       | Some Type_equal.T -> Nullary return_type
       | None -> Cons (type_, t))
  ;;

  let ( <: ) elisp_function =
    let v = elisp_function |> Value.intern in
    fun t -> wrap_unrolled t v
  ;;

  include (Value.Type : Value.Type.S)
end

module Private = struct
  let apply = apply
  let wrap_unrolled = wrap_unrolled
end
