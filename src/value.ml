open! Core_kernel
open! Import

open Value_intf

include Value0

module type Funcall          = Funcall          with type value := t
module type Make_subtype_arg = Make_subtype_arg with type value := t
module type Subtype          = Subtype          with type value := t

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

(* Exposed for use in generated bindings *)
module Expert = struct
  let raise_if_emacs_signaled () =
    match non_local_exit_get_and_clear () with
    | Return -> ()
    | Signal (symbol, data)  -> raise_s [%message "signal" (symbol : t) (data  : t)]
    | Throw  (tag,    value) -> raise_s [%message "throw"  (tag    : t) (value : t)]
  ;;
end
include Expert

let wrap_raise1 f a =
  let r = f a in
  raise_if_emacs_signaled ();
  r
;;

let wrap_raise2 f a1 a2 =
  let r = f a1 a2 in
  raise_if_emacs_signaled ();
  r
;;

let wrap_raise3 f a1 a2 a3 =
  let r = f a1 a2 a3 in
  raise_if_emacs_signaled ();
  r
;;

external intern : string -> t = "ecaml_intern"
let intern = wrap_raise1 intern

module Q = struct
  let arrayp                 = "arrayp"                 |> intern
  let bufferp                = "bufferp"                |> intern
  let car                    = "car"                    |> intern
  let cdr                    = "cdr"                    |> intern
  let commandp               = "commandp"               |> intern
  let cons                   = "cons"                   |> intern
  let consp                  = "consp"                  |> intern
  let equal                  = "equal"                  |> intern
  let floatp                 = "floatp"                 |> intern
  let fontp                  = "fontp"                  |> intern
  let format                 = "format"                 |> intern
  let framep                 = "framep"                 |> intern
  let functionp              = "functionp"              |> intern
  let hash_table_p           = "hash-table-p"           |> intern
  let integerp               = "integerp"               |> intern
  let keymapp                = "keymapp"                |> intern
  let list                   = "list"                   |> intern
  let markerp                = "markerp"                |> intern
  let nil                    = "nil"                    |> intern
  let processp               = "processp"               |> intern
  let stringp                = "stringp"                |> intern
  let symbolp                = "symbolp"                |> intern
  let syntax_table_p         = "syntax-table-p"         |> intern
  let t                      = "t"                      |> intern
  let vectorp                = "vectorp"                |> intern
  let window_configuration_p = "window-configuration-p" |> intern
  let windowp                = "windowp"                |> intern
end

external funcall_array : t -> t array -> bool -> t = "ecaml_funcall_array"

let funcall_array t ts ~should_return_result = funcall_array t ts should_return_result

let funcallN_array t ts =
  let r = funcall_array t ts ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcallN_array_i t ts =
  ignore (funcall_array t ts ~should_return_result:false : t);
  raise_if_emacs_signaled ();
;;

let funcallN t ts = funcallN_array t (ts |> Array.of_list)

let funcallN_i t ts = funcallN_array_i t (ts |> Array.of_list)

external funcall0 : t ->                bool -> t = "ecaml_funcall0"
external funcall1 : t -> t ->           bool -> t = "ecaml_funcall1"
external funcall2 : t -> t -> t ->      bool -> t = "ecaml_funcall2"
external funcall3 : t -> t -> t -> t -> bool -> t = "ecaml_funcall3"

external funcall4 : t -> t -> t -> t -> t -> bool -> t
  = "ecaml_funcall4_byte" "ecaml_funcall4"

external funcall5 : t -> t -> t -> t -> t -> t -> bool -> t
  = "ecaml_funcall5_byte" "ecaml_funcall5"

let funcall0 f ~should_return_result =
  funcall0 f should_return_result
;;

let funcall1 f a1 ~should_return_result =
  funcall1 f a1 should_return_result
;;

let funcall2 f a1 a2 ~should_return_result =
  funcall2 f a1 a2 should_return_result
;;

let funcall3 f a1 a2 a3 ~should_return_result =
  funcall3 f a1 a2 a3 should_return_result
;;

let funcall4 f a1 a2 a3 a4 ~should_return_result =
  funcall4 f a1 a2 a3 a4 should_return_result
;;

let funcall5 f a1 a2 a3 a4 a5 ~should_return_result =
  funcall5 f a1 a2 a3 a4 a5 should_return_result
;;

let funcall0_i f =
  ignore (funcall0 f ~should_return_result:false : t);
  raise_if_emacs_signaled ();
;;

let funcall0 f =
  let r = funcall0 f ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall1_i f a =
  ignore (funcall1 f a ~should_return_result:false : t);
  raise_if_emacs_signaled ();
;;

let funcall1 f a =
  let r = funcall1 f a ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall2_i f a1 a2 =
  ignore (funcall2 f a1 a2 ~should_return_result:false : t);
  raise_if_emacs_signaled ();
;;

let funcall2 f a1 a2 =
  let r = funcall2 f a1 a2 ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall3_i f a1 a2 a3 =
  ignore (funcall3 f a1 a2 a3 ~should_return_result:false : t);
  raise_if_emacs_signaled ();
;;

let funcall3 f a1 a2 a3 =
  let r = funcall3 f a1 a2 a3 ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall4_i f a1 a2 a3 a4 =
  ignore (funcall4 f a1 a2 a3 a4 ~should_return_result:false : t);
  raise_if_emacs_signaled ();
;;

let funcall4 f a1 a2 a3 a4 =
  let r = funcall4 f a1 a2 a3 a4 ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall5_i f a1 a2 a3 a4 a5 =
  ignore (funcall5 f a1 a2 a3 a4 a5 ~should_return_result:false : t);
  raise_if_emacs_signaled ();
;;

let funcall5 f a1 a2 a3 a4 a5 =
  let r = funcall5 f a1 a2 a3 a4 a5 ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

external funcall_int_int_value_unit
  : t -> int -> int -> t -> unit = "ecaml_funcall_int_int_value_unit"

let funcall_int_int_value_unit f a1 a2 a3 =
  funcall_int_int_value_unit f a1 a2 a3;
  raise_if_emacs_signaled ();
;;

external funcall_int_int_value_value_unit
  : t -> int -> int -> t -> t -> unit = "ecaml_funcall_int_int_value_value_unit"

let funcall_int_int_value_value_unit f a1 a2 a3 a4 =
  funcall_int_int_value_value_unit f a1 a2 a3 a4;
  raise_if_emacs_signaled ();
;;

external type_of : t -> t = "ecaml_type_of"
let type_of = wrap_raise1 type_of

external is_not_nil : t -> bool = "ecaml_is_not_nil"
let is_not_nil = wrap_raise1 is_not_nil

external eq : t -> t -> bool = "ecaml_eq"
let eq = wrap_raise2 eq

external of_int_exn : int -> t = "ecaml_of_int"
let of_int_exn = wrap_raise1 of_int_exn

external to_int_exn : t -> int = "ecaml_to_int"
let to_int_exn = wrap_raise1 to_int_exn

external of_float : float -> t = "ecaml_of_float"
let of_float = wrap_raise1 of_float

external to_float_exn : t -> float = "ecaml_to_float"
let to_float_exn = wrap_raise1 to_float_exn

external of_utf8_bytes : string -> t = "ecaml_of_string"
let of_utf8_bytes = wrap_raise1 of_utf8_bytes

external to_utf8_bytes_exn : t -> string = "ecaml_to_string"
let to_utf8_bytes_exn = wrap_raise1 to_utf8_bytes_exn

external vec_get  : t -> int -> t = "ecaml_vec_get"
let vec_get = wrap_raise2 vec_get

external vec_set  : t -> int -> t -> unit = "ecaml_vec_set"
let vec_set = wrap_raise3 vec_set

external vec_size : t -> int = "ecaml_vec_size"
let vec_size = wrap_raise1 vec_size

let nil = Q.nil
let t   = Q.t

let to_bool = is_not_nil

let of_bool b = if b then t else nil

let list ts = funcallN Q.list ts

let is_nil t = eq t nil

let is_array                t = funcall1 Q.arrayp                 t |> to_bool
let is_buffer               t = funcall1 Q.bufferp                t |> to_bool
let is_command              t = funcall1 Q.commandp               t |> to_bool
let is_cons                 t = funcall1 Q.consp                  t |> to_bool
let is_float                t = funcall1 Q.floatp                 t |> to_bool
let is_font                 t = funcall1 Q.fontp                  t |> to_bool
let is_frame                t = funcall1 Q.framep                 t |> to_bool
let is_function             t = funcall1 Q.functionp              t |> to_bool
let is_hash_table           t = funcall1 Q.hash_table_p           t |> to_bool
let is_integer              t = funcall1 Q.integerp               t |> to_bool
let is_keymap               t = funcall1 Q.keymapp                t |> to_bool
let is_marker               t = funcall1 Q.markerp                t |> to_bool
let is_process              t = funcall1 Q.processp               t |> to_bool
let is_string               t = funcall1 Q.stringp                t |> to_bool
let is_symbol               t = funcall1 Q.symbolp                t |> to_bool
let is_syntax_table         t = funcall1 Q.syntax_table_p         t |> to_bool
let is_vector               t = funcall1 Q.vectorp                t |> to_bool
let is_window               t = funcall1 Q.windowp                t |> to_bool
let is_window_configuration t = funcall1 Q.window_configuration_p t |> to_bool

let equal t1 t2 = funcall2 Q.equal t1 t2 |> to_bool

let cons t1 t2 = funcall2 Q.cons t1 t2

let car_exn t = funcall1 Q.car t
let cdr_exn t = funcall1 Q.cdr t

let to_list_exn (t : t) ~f =
  let rec loop t ac =
    if is_nil t
    then List.rev ac
    else if is_cons t
    then loop (cdr_exn t) (f (car_exn t) :: ac)
    else raise_s [%message "[Value.to_list] got strange value" ~_:(t : t)]
  in
  loop t []
;;

let initialize_module =
  initialize_module;
  sexp_of_t_ref := (fun t ->
    let sexp_string =
      funcall2 Q.format (of_utf8_bytes "%S" (* print as Sexp *)) t
      |> to_utf8_bytes_exn
    in
    match Sexp.of_string sexp_string with
    | sexp -> sexp
    | exception _ -> Atom sexp_string)
;;

let initialize_module =
  initialize_module;
  Ecaml_callback.(register no_active_env)
    ~f:(fun () ->
      eprint_s [%message "Ecaml called with no active env"
                           ~backtrace:(Backtrace.get () : Backtrace.t)]);
;;

module Make_subtype (M : Make_subtype_arg) = struct
  open M

  type nonrec t = t [@@deriving sexp_of]

  let to_value t = t

  let of_value_exn t =
    if not (is_in_subtype t)
    then raise_s [%message
           (concat [ "[";name;"]'s [of_value_exn] got value not in subtype" ])
             ~_:(t : t)];
    t
  ;;

  let eq (t1 : t) t2 = eq t1 t2
end

module Stat = struct
  type t =
    { emacs_free_performed : int
    ; emacs_free_scheduled : int }
  [@@deriving sexp_of]

  external num_emacs_free_performed : unit -> int = "ecaml_num_emacs_free_performed"
  external num_emacs_free_scheduled : unit -> int = "ecaml_num_emacs_free_scheduled"

  let now () =
    { emacs_free_performed = num_emacs_free_performed ()
    ; emacs_free_scheduled = num_emacs_free_scheduled () }
  ;;

  let diff t2 t1 =
    { emacs_free_performed = t2.emacs_free_performed - t1.emacs_free_performed
    ; emacs_free_scheduled = t2.emacs_free_scheduled - t1.emacs_free_scheduled }
  ;;
end
