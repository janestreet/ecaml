open! Core_kernel
open! Import

type _ t =
  | Return : 'a Value.Type.t -> 'a t
  | Return_nil : unit t
  | Cons : 'a Value.Type.t * 'b t -> ('a -> 'b) t

let return type_ = Return type_

let return_nil = Return_nil

let (@->) type_ t = Cons (type_, t)

let return_type_of_value symbol (type_ : 'a Value.Type.t) value =
  match type_.of_value_exn value with
  | x -> x
  | exception exn ->
    raise_s [%message
      "funcall failed to convert return value."
        (symbol     : Symbol.t)
        (type_.name : Sexp.t)
        (exn        : Exn.t)]
;;

let wrap : type a . a t -> Symbol.t -> a = fun t symbol ->
  let rec curry : type a . a t -> Symbol.t -> Value.t array -> int -> a
    = fun t symbol args i ->
      match t with
      | Cons (type_, t) ->
        (fun arg ->
           Array.set args i (type_.to_value arg);
           curry t symbol args (i + 1))
      | Return_nil ->
        Value.funcallN_array_i (symbol |> Symbol.to_value) args
      | Return type_ ->
        Value.funcallN_array (symbol |> Symbol.to_value) args
        |> return_type_of_value symbol type_
  in
  let rec count_args : type a . a t -> int -> int = fun t i ->
    match t with
    | Return _   -> i
    | Return_nil -> i
    | Cons (_, t) -> count_args t (i + 1)
  in
  let args = Array.create ~len:(count_args t 0) Value.nil in
  curry t symbol args 0
;;

(* It's unclear how much this sort of unrolling matters, but the C bindings do it, so we
   might as well do it here. *)
let wrap_unrolled : type a . a t -> Symbol.t -> a = fun t symbol ->
  let ret type_ value = return_type_of_value symbol type_ value in
  match t with
  | Return type_ -> Value.funcall0   (symbol |> Symbol.to_value) |> type_.of_value_exn
  | Return_nil   -> Value.funcall0_i (symbol |> Symbol.to_value)
  | Cons (type1, Return type_) ->
    (fun a1 ->
       Value.funcall1
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
       |> ret type_)
  | Cons (type1, Return_nil) ->
    (fun a1 ->
       Value.funcall1_i
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value))
  | Cons (type1, Cons (type2, Return type_)) ->
    (fun a1 a2 ->
       Value.funcall2
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
         (a2 |> type2.to_value)
       |> ret type_)
  | Cons (type1, Cons (type2, Return_nil)) ->
    (fun a1 a2 ->
       Value.funcall2_i
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
         (a2 |> type2.to_value))
  | Cons (type1, Cons (type2, Cons (type3, Return type_))) ->
    (fun a1 a2 a3 ->
       Value.funcall3
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
         (a2 |> type2.to_value)
         (a3 |> type3.to_value)
       |> ret type_)
  | Cons (type1, Cons (type2, Cons (type3, Return_nil))) ->
    (fun a1 a2 a3 ->
       Value.funcall3_i
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
         (a2 |> type2.to_value)
         (a3 |> type3.to_value))
  | Cons (type1, Cons (type2, Cons (type3, Cons (type4, Return type_)))) ->
    (fun a1 a2 a3 a4 ->
       Value.funcall4
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
         (a2 |> type2.to_value)
         (a3 |> type3.to_value)
         (a4 |> type4.to_value)
       |> ret type_)
  | Cons (type1, Cons (type2, Cons (type3, Cons (type4, Return_nil)))) ->
    (fun a1 a2 a3 a4 ->
       Value.funcall4_i
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
         (a2 |> type2.to_value)
         (a3 |> type3.to_value)
         (a4 |> type4.to_value))
  | Cons (type1, Cons (type2, Cons (type3, Cons (type4, Cons (type5, Return type_))))) ->
    (fun a1 a2 a3 a4 a5 ->
       Value.funcall5
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
         (a2 |> type2.to_value)
         (a3 |> type3.to_value)
         (a4 |> type4.to_value)
         (a5 |> type5.to_value)
       |> ret type_)
  | Cons (type1, Cons (type2, Cons (type3, Cons (type4, Cons (type5, Return_nil))))) ->
    (fun a1 a2 a3 a4 a5 ->
       Value.funcall5_i
         (symbol |> Symbol.to_value)
         (a1 |> type1.to_value)
         (a2 |> type2.to_value)
         (a3 |> type3.to_value)
         (a4 |> type4.to_value)
         (a5 |> type5.to_value))
  | t -> wrap t symbol
;;

let (<:) symbol t = wrap_unrolled t symbol

let nullary return_type symbol () =
  Value.funcall0
    (symbol |> Symbol.to_value)
  |> return_type.Value.Type.of_value_exn
;;

let nullary_nil symbol () = Value.funcall0_i (symbol |> Symbol.to_value)
