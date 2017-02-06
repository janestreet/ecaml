open! Core
open! Async
open! Import

include Value0

module Emacs_value = struct
  type nonrec t = t
end

let sexp_of_t_ref =
  ref (fun _ -> [%message [%here] "sexp_of_t not yet defined"])
;;

let sexp_of_t t = !sexp_of_t_ref t

(* [Funcall_exit.t] values are only constructed by [ecaml_non_local_exit_get_and_clear]
   in [ecaml_stubs.c]. *)
module Funcall_exit = struct
  type t =
    | Return
    | Signal of Emacs_value.t * Emacs_value.t
    | Throw  of Emacs_value.t * Emacs_value.t
  [@@deriving variants]

  let initialize_module =
    assert (Variants.return.rank = 0);
    assert (Variants.signal.rank = 1);
    assert (Variants.throw .rank = 2);
  ;;
end

let initialize_module = Funcall_exit.initialize_module

external non_local_exit_get_and_clear : unit -> Funcall_exit.t =
  "ecaml_non_local_exit_get_and_clear"
;;

let raise_if_emacs_signaled () =
  match non_local_exit_get_and_clear () with
  | Return -> ()
  | Signal (symbol, data)  -> raise_s [%message "signal" (symbol : t) (data  : t)]
  | Throw  (tag,    value) -> raise_s [%message "throw"  (tag    : t) (value : t)]
;;

let unary f a =
  let r = f a in
  raise_if_emacs_signaled ();
  r
;;

let binary f a1 a2 =
  let r = f a1 a2 in
  raise_if_emacs_signaled ();
  r
;;

let ternary f a1 a2 a3 =
  let r = f a1 a2 a3 in
  raise_if_emacs_signaled ();
  r
;;

(* We register [attach_async_finalizer] here because it is needed to create any
   [Value.t]s. *)
let initialize_module =
  initialize_module;
  let module M = struct
    external free_emacs_value : t -> unit = "ecaml_free_emacs_value"
  end in
  Ecaml_callback.(register attach_async_finalizer)
    ~should_run_holding_async_lock:true
    ~f:(fun v -> Gc.add_finalizer_exn v M.free_emacs_value);
;;

external funcall : t -> t array -> t = "ecaml_funcall"
let funcall = binary funcall
let funcall t ts = funcall t (ts |> Array.of_list)

let funcall_i f args = ignore (funcall f args : t)

external intern : string -> t = "ecaml_intern"
let intern = unary intern

external type_of : t -> t = "ecaml_type_of"
let type_of = unary type_of

external is_not_nil : t -> bool = "ecaml_is_not_nil"
let is_not_nil = unary is_not_nil

external eq : t -> t -> bool = "ecaml_eq"
let eq = binary eq

external of_int : int -> t = "ecaml_of_int"
let of_int = unary of_int

external to_int : t -> int = "ecaml_to_int"
let to_int = unary to_int

external of_float : float -> t = "ecaml_of_float"
let of_float = unary of_float

external to_float : t -> float = "ecaml_to_float"
let to_float = unary to_float

external of_string : string -> t = "ecaml_of_string"
let of_string = unary of_string

external to_string : t -> string = "ecaml_to_string"
let to_string = unary to_string

external vec_get  : t -> int -> t = "ecaml_vec_get"
let vec_get = binary vec_get

external vec_set  : t -> int -> t -> unit = "ecaml_vec_set"
let vec_set = ternary vec_set

external vec_size : t -> int = "ecaml_vec_size"
let vec_size = unary vec_size

let nil = intern "nil"

module Symbol = struct
  let format  = intern "format"
  let list    = intern "list"
  let symbolp = intern "symbolp"
  let t       = intern "t"
end

let list ts = funcall Symbol.list ts

let is_symbol t = eq (funcall Symbol.symbolp [ t ]) Symbol.t

let initialize_module =
  initialize_module;
  sexp_of_t_ref := (fun t ->
    let sexp_string =
      funcall Symbol.format [ of_string "%S" (* print as Sexp *); t ]
      |> to_string
    in
    match Sexp.of_string sexp_string with
    | sexp -> sexp
    | exception _ -> Atom sexp_string)
;;

let initialize_module =
  initialize_module;
  Ecaml_callback.(register no_active_env)
    ~should_run_holding_async_lock:false
    ~f:(fun () ->
      eprint_s [%message "Ecaml called with no active env"
                           ~backtrace:(Backtrace.get () : Backtrace.t)]);
;;
