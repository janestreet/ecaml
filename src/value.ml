open! Core_kernel
open! Import0

open Value_intf

include Value0

type value = t

module type Funcall          = Funcall          with type value := t
module type Make_subtype_arg = Make_subtype_arg with type value := t

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

exception Elisp_signal of { symbol : t; data : t }

let raise_if_emacs_signaled () =
  match non_local_exit_get_and_clear () with
  | Return -> ()
  | Signal (symbol, data)
  | Throw  (symbol, data) -> raise (Elisp_signal { symbol; data })
;;

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
  let append                 = "append"                 |> intern
  let arrayp                 = "arrayp"                 |> intern
  let bufferp                = "bufferp"                |> intern
  let car                    = "car"                    |> intern
  let cadr                   = "cadr"                   |> intern
  let cdr                    = "cdr"                    |> intern
  let commandp               = "commandp"               |> intern
  let cons                   = "cons"                   |> intern
  let consp                  = "consp"                  |> intern
  let debug_on_error         = "debug-on-error"         |> intern
  let equal                  = "equal"                  |> intern
  let error                  = "error"                  |> intern
  let eventp                 = "eventp"                 |> intern
  let floatp                 = "floatp"                 |> intern
  let fontp                  = "fontp"                  |> intern
  let framep                 = "framep"                 |> intern
  let functionp              = "functionp"              |> intern
  let hash_table_p           = "hash-table-p"           |> intern
  let keymapp                = "keymapp"                |> intern
  let list                   = "list"                   |> intern
  let markerp                = "markerp"                |> intern
  let nil                    = "nil"                    |> intern
  let prin1_to_string        = "prin1-to-string"        |> intern
  let processp               = "processp"               |> intern
  let stringp                = "stringp"                |> intern
  let symbol_value           = "symbol-value"           |> intern
  let symbolp                = "symbolp"                |> intern
  let syntax_table_p         = "syntax-table-p"         |> intern
  let t                      = "t"                      |> intern
  let timerp                 = "timerp"                 |> intern
  let vector                 = "vector"                 |> intern
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

let is_integer (t : t) = Obj.is_int (Obj.repr t)

let to_int_exn (t : t) =
  if not (is_integer t) then
    raise_s [%sexp ("wrong-type-argument", ("integerp", (t : t)))]
  else
    (Obj.magic t : int)
;;

let get_int_var string = funcall1 Q.symbol_value (string |> intern) |> to_int_exn
let debug_on_error () = is_not_nil (funcall1 Q.symbol_value Q.debug_on_error)

let emacs_min_int = get_int_var "most-negative-fixnum"
let emacs_max_int = get_int_var "most-positive-fixnum"

let of_int_exn : int -> t =
  let check_bounds =
    Int.validate_bound
      ~min:(Incl emacs_min_int)
      ~max:(Incl emacs_max_int)
  in
  fun n ->
    Validate.maybe_raise
      (Validate.name "overflow-error" (check_bounds n));
    (Obj.magic n : t);
;;

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

let option to_value = Option.value_map ~default:nil ~f:to_value

let to_bool = is_not_nil

let of_bool b = if b then t else nil

let list ts = funcallN Q.list ts

let is_nil t = eq t nil

let is_array                t = funcall1 Q.arrayp                 t |> to_bool
let is_buffer               t = funcall1 Q.bufferp                t |> to_bool
let is_command              t = funcall1 Q.commandp               t |> to_bool
let is_event                t = funcall1 Q.eventp                 t |> to_bool
let is_float                t = funcall1 Q.floatp                 t |> to_bool
let is_font                 t = funcall1 Q.fontp                  t |> to_bool
let is_frame                t = funcall1 Q.framep                 t |> to_bool
let is_function             t = funcall1 Q.functionp              t |> to_bool
let is_hash_table           t = funcall1 Q.hash_table_p           t |> to_bool
let is_keymap               t = funcall1 Q.keymapp                t |> to_bool
let is_marker               t = funcall1 Q.markerp                t |> to_bool
let is_process              t = funcall1 Q.processp               t |> to_bool
let is_string               t = funcall1 Q.stringp                t |> to_bool
let is_symbol               t = funcall1 Q.symbolp                t |> to_bool
let is_syntax_table         t = funcall1 Q.syntax_table_p         t |> to_bool
let is_timer                t = funcall1 Q.timerp                 t |> to_bool
let is_vector               t = funcall1 Q.vectorp                t |> to_bool
let is_window               t = funcall1 Q.windowp                t |> to_bool
let is_window_configuration t = funcall1 Q.window_configuration_p t |> to_bool

let equal t1 t2 = funcall2 Q.equal t1 t2 |> to_bool

let cons t1 t2 = funcall2 Q.cons t1 t2

let car_exn  t = funcall1 Q.car  t
let cdr_exn  t = funcall1 Q.cdr  t
let cadr_exn t = funcall1 Q.cadr t

let is_cons ?car ?cdr t =
  funcall1 Q.consp t |> to_bool
  && (match car with
    | None -> true
    | Some is_car -> is_car (car_exn t))
  && (match cdr with
    | None -> true
    | Some is_cdr -> is_cdr (cdr_exn t))
;;

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

let to_array_exn (t : t) ~f =
  let length = vec_size t in
  Array.init length ~f:(fun i -> f (vec_get t i))
;;

let vector arr = funcallN_array Q.vector arr

let looks_nice_in_symbol = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' | '-'
  | '!' | '*' | '+' | '/' | ':' | '<' | '=' | '>' | '@' | '|'
    -> true
  | _ -> false
;;

let rec pretty_sexp : Sexp.t -> t = function
  | Atom s ->
    if String.for_all s ~f:looks_nice_in_symbol
    then intern s
    else s |> of_utf8_bytes
  | List sexps -> list (List.map sexps ~f:pretty_sexp)
;;

let non_local_exit_signal exn =
  let module M = struct
    (** [non_local_exit_signal] sets a [pending_error] flag in the Emacs environment that
        causes it to, after our C code returns to it, signal instead of returning a
        value. *)
    external non_local_exit_signal : t -> t -> unit =
      "ecaml_non_local_exit_signal" end in
  let symbol, data =
    let append_backtrace_if_debugging rest =
      match debug_on_error () with
      | true ->
        [%sexp { backtrace : Backtrace.t = Backtrace.Exn.most_recent () }]
        |> pretty_sexp
        |> funcall2 Q.append rest
      | false -> rest
    in
    match exn with
    | Elisp_signal { symbol; data } ->
      (* This case preserves an Elisp signal as it crosses an OCaml boundary. *)
      symbol, append_backtrace_if_debugging data
    | _ ->
      let string, rest =
        match [%sexp (exn : exn)] with
        | Atom string                  -> string , []
        | List [ Atom string ]         -> string , []
        | List [ Atom string; rest ]   -> string , [ rest |> pretty_sexp ]
        | List ( Atom string :: rest ) -> string , [ List rest |> pretty_sexp ]
        | List rest                    -> "error", [ List rest |> pretty_sexp ] in
      (* For the [error] symbol, the error data should be a list whose car is a string.
         See [(Info-goto-node "(elisp)Signaling Errors")]. *)
      Q.error, append_backtrace_if_debugging (list ((string |> of_utf8_bytes) :: rest)) in
  M.non_local_exit_signal symbol data
;;

let initialize_module =
  initialize_module;
  Sexplib.Conv.Exn_converter.add [%extension_constructor Elisp_signal]
    (function
      | Elisp_signal { symbol; data } ->
        if eq symbol Q.error
        then [%sexp (data : t)]
        else if is_nil data
        then [%sexp (symbol : t)]
        else [%message "" ~_:(symbol : t) ~_:(data : t)]
      | _ ->
        (* Reaching this branch indicates a bug in sexplib. *)
        assert false);
  sexp_of_t_ref := (fun t ->
    let sexp_string =
      funcall1 Q.prin1_to_string t
      |> to_utf8_bytes_exn
    in
    let sexp_string =
      (* Emacs prefixes some values (like buffers, markers, etc) with [#], which then
         makes the sexp unparseable. So in this case we strip the [#]. *)
      if String.is_prefix sexp_string ~prefix:"#("
      then String.chop_prefix_exn sexp_string ~prefix:"#"
      else sexp_string
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

module Type = struct
  type 'a t =
    { name         : Sexp.t
    ; of_value_exn : value -> 'a
    ; to_value     : 'a -> value }

  let sexp_of_t _ { name; of_value_exn = _; to_value = _ } = name

  let bool =
    { name         = [%message "bool"]
    ; of_value_exn = to_bool
    ; to_value     = of_bool }
  ;;

  let ignored =
    { name         = [%message "ignored"]
    ; of_value_exn = ignore
    ; to_value     = const nil }
  ;;

  let int =
    { name         = [%message "int"]
    ; of_value_exn = to_int_exn
    ; to_value     = of_int_exn }
  ;;

  let string =
    { name         = [%message "string"]
    ; of_value_exn = to_utf8_bytes_exn
    ; to_value     = of_utf8_bytes }
  ;;

  let unit =
    { name         = [%message "unit"]
    ; of_value_exn = (fun value -> assert (is_nil value))
    ; to_value     = const nil }
  ;;


  let value =
    { name         = [%message "value"]
    ; of_value_exn = Fn.id
    ; to_value     = Fn.id }
  ;;

  let alist t1 t2 =
    { name         = [%message "alist" ~_:(t1.name : Sexp.t) ~_:(t2.name : Sexp.t)]
    ; of_value_exn = to_list_exn ~f:(fun cons_cell ->
        (t1.of_value_exn (car_exn cons_cell),
         t2.of_value_exn (cdr_exn cons_cell)))
    ; to_value     = fun l ->
        list (List.map l ~f:(fun (a,b) ->
          cons (t1.to_value a) (t2.to_value b))) }
  ;;

  let list t =
    { name         = [%message "list" ~_:(t.name : Sexp.t)]
    ; of_value_exn = to_list_exn ~f:t.of_value_exn
    ; to_value     = fun l -> list (List.map l ~f:t.to_value) }
  ;;

  let vector t =
    { name         = [%message "vector" ~_:(t.name : Sexp.t)]
    ; of_value_exn = to_array_exn ~f:t.of_value_exn
    ; to_value     = fun a -> vector (Array.map a ~f:t.to_value) }
  ;;

  let tuple t1 t2 =
    { name = Sexp.List [t1.name; t2.name]
    ; of_value_exn = (fun v -> t1.of_value_exn (car_exn v), t2.of_value_exn (cdr_exn v))
    ; to_value     = (fun (a1, a2) -> cons (t1.to_value a1) (t2.to_value a2))
    }
  ;;

  let tuple2_as_list t1 t2 =
    { name = Sexp.List [t1.name; t2.name]
    ; of_value_exn = (fun v -> t1.of_value_exn (car_exn v), t2.of_value_exn (cadr_exn v))
    ; to_value     = (fun (a1, a2) -> cons (t1.to_value a1) (cons (t2.to_value a2) nil))
    }
  ;;

  let option t =
    { name         = [%message "option" ~_:(t.name : Sexp.t)]
    ; of_value_exn = (fun v -> if is_nil v then None else Some (v |> t.of_value_exn))
    ; to_value     = option t.to_value }
  ;;

  (* embed ocaml values as strings which are sexp representations *)
  let sexpable (type a) (module A : Sexpable with type t = a) ~name =
    { name = [%message "sexpable" ~_:(name : Sexp.t)]
    ; of_value_exn = (fun v -> A.t_of_sexp (Sexp.of_string (string.of_value_exn v)))
    ; to_value = (fun a -> A.sexp_of_t a |> Sexp.to_string_mach |> string.to_value)
    }

  let caml_embed (type_id : _ Type_equal.Id.t) =
    { name = [%message "caml_embed" (type_id : _ Type_equal.Id.t)]
    ; of_value_exn = (fun v -> let embed = Caml_embed.of_value_exn v in
                       Caml_embed.extract_exn embed type_id)
    ; to_value = (fun a -> Caml_embed.create type_id a |> Caml_embed.to_value)
    }

  let map t ~name ~of_ ~to_ =
    { name
    ; of_value_exn = Fn.compose of_        t.of_value_exn
    ; to_value     = Fn.compose t.to_value to_ }
  ;;

  let path_list =
    option string
    |> map ~name:[%message "path-list-element"]
         ~of_:(Option.value ~default:".")
         ~to_:Option.return
    |> list
  ;;

end

module type Subtype = Subtype
  with type value := t
  with type 'a type_ := 'a Type.t

module Make_subtype (M : Make_subtype_arg) = struct
  open M

  type nonrec t = t [@@deriving sexp_of]

  let is_in_subtype = is_in_subtype

  let to_value t = t

  let of_value_exn t =
    if not (is_in_subtype t)
    then raise_s [%message
           (concat [ "[";name;"]'s [of_value_exn] got value not in subtype" ])
             ~_:(t : t)];
    t
  ;;

  let type_ : t Type.t = { of_value_exn; name = [%message name]; to_value }

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

module Expert = struct
  let non_local_exit_signal   = non_local_exit_signal
  let raise_if_emacs_signaled = raise_if_emacs_signaled
end

module For_testing = struct
  let map_elisp_signal g ~f =
    match g () with
    | a -> a
    | exception Elisp_signal { symbol; data } ->
      Nothing.unreachable_code (f ~symbol ~data ~reraise:(fun ~symbol ~data ->
        raise (Elisp_signal { symbol; data })))
  ;;
  let map_elisp_signal_omit_data f =
    map_elisp_signal f ~f:(fun ~symbol ~data:_ ~reraise -> reraise ~symbol ~data:nil)
  ;;
end
