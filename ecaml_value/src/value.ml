open! Core_kernel
open! Import
open Value_intf
include Value0

module type Funcall = Funcall with type value := t
module type Make_subtype_arg = Make_subtype_arg with type value := t

module Emacs_value = struct
  type nonrec t = t
end

type value = t [@@deriving sexp_of]

(* [Funcall_exit.t] values are only constructed by [ecaml_non_local_exit_get_and_clear]
   in [ecaml_stubs.c]. *)
module Funcall_exit = struct
  type t =
    | Return
    | Signal of Emacs_value.t * Emacs_value.t
    | Throw of Emacs_value.t * Emacs_value.t
  [@@deriving variants]

  let initialize_module =
    assert (Variants.return.rank = 0);
    assert (Variants.signal.rank = 1);
    assert (Variants.throw.rank = 2)
  ;;
end

let initialize_module = Funcall_exit.initialize_module

external have_active_env : unit -> bool = "ecaml_have_active_env"

external non_local_exit_get_and_clear
  :  unit
    -> Funcall_exit.t
  = "ecaml_non_local_exit_get_and_clear"

exception Elisp_signal of { symbol : t; data : t }

let raise_if_emacs_signaled () =
  match non_local_exit_get_and_clear () with
  | Return -> ()
  | Signal (symbol, data)
  | Throw (symbol, data) -> raise (Elisp_signal { symbol; data })
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
  let append = "append" |> intern
  and arrayp = "arrayp" |> intern
  and backtrace = "backtrace" |> intern
  and bufferp = "bufferp" |> intern
  and car = "car" |> intern
  and cadr = "cadr" |> intern
  and cdr = "cdr" |> intern
  and commandp = "commandp" |> intern
  and cons = "cons" |> intern
  and consp = "consp" |> intern
  and debug_on_error = "debug-on-error" |> intern
  and equal = "equal" |> intern
  and equal_including_properties = "equal-including-properties" |> intern
  and error = "error" |> intern
  and eventp = "eventp" |> intern
  and floatp = "floatp" |> intern
  and fontp = "fontp" |> intern
  and framep = "framep" |> intern
  and functionp = "functionp" |> intern
  and hash_table_p = "hash-table-p" |> intern
  and keymapp = "keymapp" |> intern
  and list = "list" |> intern
  and markerp = "markerp" |> intern
  and message = "message" |> intern
  and nil = "nil" |> intern
  and prin1_to_string = "prin1-to-string" |> intern
  and processp = "processp" |> intern
  and stringp = "stringp" |> intern
  and substring_no_properties = "substring-no-properties" |> intern
  and symbol_value = "symbol-value" |> intern
  and symbolp = "symbolp" |> intern
  and syntax_table_p = "syntax-table-p" |> intern
  and t = "t" |> intern
  and timerp = "timerp" |> intern
  and vector = "vector" |> intern
  and vectorp = "vectorp" |> intern
  and window_configuration_p = "window-configuration-p" |> intern
  and windowp = "windowp" |> intern
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
  raise_if_emacs_signaled ()
;;

let funcallN t ts = funcallN_array t (ts |> Array.of_list)
let funcallN_i t ts = funcallN_array_i t (ts |> Array.of_list)

external funcall0 : t -> bool -> t = "ecaml_funcall0"
external funcall1 : t -> t -> bool -> t = "ecaml_funcall1"
external funcall2 : t -> t -> t -> bool -> t = "ecaml_funcall2"
external funcall3 : t -> t -> t -> t -> bool -> t = "ecaml_funcall3"

external funcall4
  :  t
    -> t
    -> t
    -> t
    -> t
    -> bool
    -> t
  = "ecaml_funcall4_byte" "ecaml_funcall4"

external funcall5
  :  t
    -> t
    -> t
    -> t
    -> t
    -> t
    -> bool
    -> t
  = "ecaml_funcall5_byte" "ecaml_funcall5"

let funcall0 f ~should_return_result = funcall0 f should_return_result
let funcall1 f a1 ~should_return_result = funcall1 f a1 should_return_result
let funcall2 f a1 a2 ~should_return_result = funcall2 f a1 a2 should_return_result
let funcall3 f a1 a2 a3 ~should_return_result = funcall3 f a1 a2 a3 should_return_result

let funcall4 f a1 a2 a3 a4 ~should_return_result =
  funcall4 f a1 a2 a3 a4 should_return_result
;;

let funcall5 f a1 a2 a3 a4 a5 ~should_return_result =
  funcall5 f a1 a2 a3 a4 a5 should_return_result
;;

let funcall0_i f =
  ignore (funcall0 f ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall0 f =
  let r = funcall0 f ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall1_i f a =
  ignore (funcall1 f a ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall1 f a =
  let r = funcall1 f a ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall2_i f a1 a2 =
  ignore (funcall2 f a1 a2 ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall2 f a1 a2 =
  let r = funcall2 f a1 a2 ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall3_i f a1 a2 a3 =
  ignore (funcall3 f a1 a2 a3 ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall3 f a1 a2 a3 =
  let r = funcall3 f a1 a2 a3 ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall4_i f a1 a2 a3 a4 =
  ignore (funcall4 f a1 a2 a3 a4 ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall4 f a1 a2 a3 a4 =
  let r = funcall4 f a1 a2 a3 a4 ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall5_i f a1 a2 a3 a4 a5 =
  ignore (funcall5 f a1 a2 a3 a4 a5 ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall5 f a1 a2 a3 a4 a5 =
  let r = funcall5 f a1 a2 a3 a4 a5 ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

external funcall_int_int_value_unit
  :  t
    -> int
    -> int
    -> t
    -> unit
  = "ecaml_funcall_int_int_value_unit"

let funcall_int_int_value_unit f a1 a2 a3 =
  funcall_int_int_value_unit f a1 a2 a3;
  raise_if_emacs_signaled ()
;;

external funcall_int_int_value_value_unit
  :  t
    -> int
    -> int
    -> t
    -> t
    -> unit
  = "ecaml_funcall_int_int_value_value_unit"

let funcall_int_int_value_value_unit f a1 a2 a3 a4 =
  funcall_int_int_value_value_unit f a1 a2 a3 a4;
  raise_if_emacs_signaled ()
;;

external type_of : t -> t = "ecaml_type_of"

let type_of = wrap_raise1 type_of

external is_not_nil : t -> bool = "ecaml_is_not_nil"

let is_not_nil = wrap_raise1 is_not_nil

external eq : t -> t -> bool = "ecaml_eq"

let eq = wrap_raise2 eq
let[@inline always] is_integer (t : t) = Obj.is_int (Obj.repr t)

let[@inline always] to_int_exn (t : t) =
  if not (is_integer t)
  then raise_s [%sexp "wrong-type-argument", ("integerp", (t : t))]
  else (Obj.magic t : int)
;;

let get_int_var string = funcall1 Q.symbol_value (string |> intern) |> to_int_exn
let debug_on_error () = is_not_nil (funcall1 Q.symbol_value Q.debug_on_error)
let emacs_min_int = get_int_var "most-negative-fixnum"
let emacs_max_int = get_int_var "most-positive-fixnum"

let of_int_exn : int -> t =
  let check_bounds =
    Int.validate_bound ~min:(Incl emacs_min_int) ~max:(Incl emacs_max_int)
  in
  fun n ->
    Validate.maybe_raise (Validate.name "overflow-error" (check_bounds n));
    (Obj.magic n : t)
;;

external of_float : float -> t = "ecaml_of_float"

let of_float = wrap_raise1 of_float

external to_float_exn : t -> float = "ecaml_to_float"

let to_float_exn = wrap_raise1 to_float_exn

external of_utf8_bytes : string -> t = "ecaml_of_string"

let of_utf8_bytes = wrap_raise1 of_utf8_bytes
let of_utf8_bytes_cache = Hashtbl.create (module String)

let of_utf8_bytes_cached string =
  Hashtbl.find_or_add of_utf8_bytes_cache string ~default:(fun () -> of_utf8_bytes string)
;;

external to_utf8_bytes_exn : t -> string = "ecaml_to_string"

let to_utf8_bytes_exn = wrap_raise1 to_utf8_bytes_exn

external vec_get : t -> int -> t = "ecaml_vec_get"

let vec_get = wrap_raise2 vec_get

external vec_set : t -> int -> t -> unit = "ecaml_vec_set"

let vec_set = wrap_raise3 vec_set

external vec_size : t -> int = "ecaml_vec_size"

let vec_size = wrap_raise1 vec_size
let percent_s = of_utf8_bytes "%s"
let message s = funcall2_i Q.message percent_s (of_utf8_bytes s)
let messagef fmt = ksprintf message fmt

let message_s : Sexp.t -> unit = function
  | Atom string -> message string
  | sexp -> messagef "%s" (sexp |> Sexp.to_string_hum)
;;

let nil = Q.nil
let t = Q.t
let option to_value = Option.value_map ~default:nil ~f:to_value
let to_bool = is_not_nil
let of_bool b = if b then t else nil
let list ts = funcallN Q.list ts
let is_nil t = eq t nil
let is_array t = funcall1 Q.arrayp t |> to_bool
let is_buffer t = funcall1 Q.bufferp t |> to_bool
let is_command t = funcall1 Q.commandp t |> to_bool
let is_event t = funcall1 Q.eventp t |> to_bool
let is_float t = funcall1 Q.floatp t |> to_bool
let is_font t = funcall1 Q.fontp t |> to_bool
let is_frame t = funcall1 Q.framep t |> to_bool
let is_function t = funcall1 Q.functionp t |> to_bool
let is_hash_table t = funcall1 Q.hash_table_p t |> to_bool
let is_keymap t = funcall1 Q.keymapp t |> to_bool
let is_marker t = funcall1 Q.markerp t |> to_bool
let is_process t = funcall1 Q.processp t |> to_bool
let is_string t = funcall1 Q.stringp t |> to_bool
let is_symbol t = funcall1 Q.symbolp t |> to_bool
let is_syntax_table t = funcall1 Q.syntax_table_p t |> to_bool
let is_timer t = funcall1 Q.timerp t |> to_bool
let is_vector t = funcall1 Q.vectorp t |> to_bool
let is_window t = funcall1 Q.windowp t |> to_bool
let is_window_configuration t = funcall1 Q.window_configuration_p t |> to_bool
let equal t1 t2 = funcall2 Q.equal t1 t2 |> to_bool
let cons t1 t2 = funcall2 Q.cons t1 t2
let car_exn t = funcall1 Q.car t
let cdr_exn t = funcall1 Q.cdr t
let cadr_exn t = funcall1 Q.cadr t

let is_cons ?car ?cdr t =
  funcall1 Q.consp t |> to_bool
  && (match car with
    | None -> true
    | Some is_car -> is_car (car_exn t))
  &&
  match cdr with
  | None -> true
  | Some is_cdr -> is_cdr (cdr_exn t)
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

let non_local_exit_signal exn =
  let module M = struct
    (** [non_local_exit_signal] sets a [pending_error] flag in the Emacs environment that
        causes it to, after our C code returns to it, signal instead of returning a
        value. *)
    external non_local_exit_signal : t -> t -> unit = "ecaml_non_local_exit_signal"
  end
  in
  let debug_on_error = debug_on_error () in
  let symbol, data =
    match exn with
    | Elisp_signal { symbol; data } ->
      (* This case preserves an Elisp signal as it crosses an OCaml boundary. *)
      let data =
        match debug_on_error with
        | false -> data
        | true ->
          funcall2
            Q.append
            data
            (list
               [ list
                   [ Q.backtrace
                   ; Backtrace.Exn.most_recent () |> Backtrace.to_string |> of_utf8_bytes
                   ]
               ])
      in
      symbol, data
    | _ ->
      let backtrace =
        if debug_on_error then Some (Backtrace.Exn.most_recent ()) else None
      in
      let message =
        [%message.omit_nil "" ~_:(exn : exn) (backtrace : Backtrace.t option)]
      in
      let message =
        match message with
        | Atom string -> string
        | List _ as sexp -> Sexp.to_string_hum sexp
      in
      (* For the [error] symbol, the error data should be a list whose car is a string.
         See [(Info-goto-node "(elisp)Signaling Errors")]. *)
      Q.error, list [ message |> of_utf8_bytes ]
  in
  M.non_local_exit_signal symbol data
;;

let prin1_to_string t = funcall1 Q.prin1_to_string t |> to_utf8_bytes_exn

let text_has_properties t =
  is_nil (funcall2 Q.equal_including_properties t (funcall1 Q.substring_no_properties t))
;;

let might_be_a_sexp string =
  let string = string |> String.strip in
  let n = String.length string in
  n >= 2 && Char.equal string.[0] '(' && Char.equal string.[n - 1] ')'
;;

let rec sexp_of_t t : Sexp.t =
  if is_string t && not (text_has_properties t)
  then (
    let string = t |> to_utf8_bytes_exn in
    if not (might_be_a_sexp string)
    then Atom string
    else (
      match string |> Sexp.of_string with
      | x -> x
      | exception _ -> Atom string))
  else if is_cons t
  then (
    let car = sexp_of_t (car_exn t) in
    let cdr = sexp_of_t (cdr_exn t) in
    match cdr with
    | Atom "nil" -> List [ car ]
    | Atom _ -> List [ car; Atom "."; cdr ]
    | List sexps -> List (car :: sexps))
  else (
    let sexp_string = prin1_to_string t in
    let sexp_string =
      (* Emacs prefixes some values (like buffers, markers, etc) with [#], which then
         makes the sexp unparseable.  So in this case we strip the [#]. *)
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
  Sexplib.Conv.Exn_converter.add [%extension_constructor Elisp_signal] (function
    | Elisp_signal { symbol; data } ->
      if eq symbol Q.error
      then [%sexp (data : t)]
      else if is_nil data
      then [%sexp (symbol : t)]
      else [%message "" ~_:(symbol : t) ~_:(data : t)]
    | _ -> (* Reaching this branch indicates a bug in sexplib. *)
      assert false);
  sexp_of_t_ref := sexp_of_t
;;

let initialize_module =
  initialize_module;
  Ecaml_callback.(register no_active_env)
    ~f:(fun () ->
      eprint_s
        [%message
          "Ecaml called with no active env" ~backtrace:(Backtrace.get () : Backtrace.t)])
    ~should_run_holding_async_lock:true
;;

module Type = struct
  type 'a t =
    { id : 'a Type_equal.Id.t
    ; of_value_exn : value -> 'a
    ; to_value : 'a -> value
    }
  [@@deriving fields]

  module type S = Type with type 'a t := 'a t with type value := value

  let name t = Sexp.of_string (Type_equal.Id.name t.id)
  let to_sexp t = Type_equal.Id.to_sexp t.id
  let sexp_of_t _ t = name t

  let create name sexp_of_t of_value_exn to_value =
    { id = Type_equal.Id.create ~name:(Sexp.to_string name) sexp_of_t
    ; of_value_exn =
        (fun value ->
           try of_value_exn value with
           | exn ->
             raise_s
               [%message
                 "unable to convert Elisp value to OCaml value"
                   ~type_:(name : Sexp.t)
                   (value : value)
                   (exn : exn)])
    ; to_value
    }
  ;;

  let with_of_value_exn t of_value_exn = { t with of_value_exn }

  module type Enum = Enum

  let enum (type a) name (module M : Enum with type t = a) to_value =
    let valid_values = List.map M.all ~f:(fun m -> to_value m, m) in
    let of_value_exn value =
      match List.Assoc.find valid_values value ~equal with
      | None -> raise_s [%message (valid_values : (value * M.t) list)]
      | Some m -> m
    in
    create name [%sexp_of: M.t] of_value_exn to_value
  ;;

  let bool = create [%message "bool"] [%sexp_of: bool] to_bool of_bool
  let float = create [%message "float"] [%sexp_of: float] to_float_exn of_float
  let ignored = create [%message "ignored"] [%sexp_of: unit] ignore (const nil)
  let int = create [%message "int"] [%sexp_of: int] to_int_exn of_int_exn

  let string =
    create [%message "string"] [%sexp_of: string] to_utf8_bytes_exn of_utf8_bytes
  ;;

  let string_cached =
    create [%message "string"] [%sexp_of: string] to_utf8_bytes_exn of_utf8_bytes_cached
  ;;

  let unit =
    create
      [%message "unit"]
      [%sexp_of: unit]
      (fun value -> assert (is_nil value))
      (const nil)
  ;;

  let value = create [%message "value"] [%sexp_of: value] Fn.id Fn.id

  let alist t1 t2 =
    create
      [%message "alist" ~_:(name t1 : Sexp.t) ~_:(name t2 : Sexp.t)]
      (sexp_of_list (Tuple2.sexp_of_t (to_sexp t1) (to_sexp t2)))
      (to_list_exn ~f:(fun cons_cell ->
         t1.of_value_exn (car_exn cons_cell), t2.of_value_exn (cdr_exn cons_cell)))
      (fun l -> list (List.map l ~f:(fun (a, b) -> cons (t1.to_value a) (t2.to_value b))))
  ;;

  let list t =
    create
      [%message "list" ~_:(name t : Sexp.t)]
      (sexp_of_list (to_sexp t))
      (to_list_exn ~f:t.of_value_exn)
      (fun l -> list (List.map l ~f:t.to_value))
  ;;

  let vector t =
    create
      [%message "vector" ~_:(name t : Sexp.t)]
      (sexp_of_array (to_sexp t))
      (to_array_exn ~f:t.of_value_exn)
      (fun a -> vector (Array.map a ~f:t.to_value))
  ;;

  let tuple t1 t2 =
    create
      [%sexp (name t1 : Sexp.t), (name t2 : Sexp.t)]
      (Tuple2.sexp_of_t (to_sexp t1) (to_sexp t2))
      (fun v -> t1.of_value_exn (car_exn v), t2.of_value_exn (cdr_exn v))
      (fun (a1, a2) -> cons (t1.to_value a1) (t2.to_value a2))
  ;;

  let tuple2_as_list t1 t2 =
    create
      [%sexp (name t1 : Sexp.t), (name t2 : Sexp.t)]
      (Tuple2.sexp_of_t (to_sexp t1) (to_sexp t2))
      (fun v -> t1.of_value_exn (car_exn v), t2.of_value_exn (cadr_exn v))
      (fun (a1, a2) -> cons (t1.to_value a1) (cons (t2.to_value a2) nil))
  ;;

  let map t ~name ~of_ ~to_ =
    create
      name
      (Fn.compose (to_sexp t) to_)
      (Fn.compose of_ t.of_value_exn)
      (Fn.compose t.to_value to_)
  ;;

  let map_id t name = map t ~name ~of_:Fn.id ~to_:Fn.id

  let option ?(wrapped = false) t =
    let t =
      if not wrapped
      then t
      else
        map
          (tuple t unit)
          ~name:[%message "wrapped" ~_:(name t : Sexp.t)]
          ~of_:(fun (a, ()) -> a)
          ~to_:(fun a -> a, ())
    in
    create
      [%message "option" ~_:(name t : Sexp.t)]
      (sexp_of_option (to_sexp t))
      (fun v -> if is_nil v then None else Some (v |> t.of_value_exn))
      (option t.to_value)
  ;;

  (* embed ocaml values as strings which are sexp representations *)
  let sexpable (type a) (module A : Sexpable with type t = a) ~name =
    create
      [%message "sexpable" ~_:(name : Sexp.t)]
      [%sexp_of: A.t]
      (fun v -> A.t_of_sexp (Sexp.of_string (string.of_value_exn v)))
      (fun a -> A.sexp_of_t a |> Sexp.to_string_mach |> string.to_value)
  ;;

  let caml_embed (type_id : _ Type_equal.Id.t) =
    create
      [%message "caml_embed" ~type_id:(Type_equal.Id.name type_id : string)]
      (Type_equal.Id.to_sexp type_id)
      (fun v ->
         let embed = Caml_embed.of_value_exn v in
         Caml_embed.extract_exn embed type_id)
      (fun a -> Caml_embed.create type_id a |> Caml_embed.to_value)
  ;;

  let path_list =
    map
      (option string)
      ~name:[%message "path-list-element"]
      ~of_:(Option.value ~default:".")
      ~to_:Option.return
    |> list
  ;;
end

module type Subtype = Subtype with type value := t with type 'a type_ := 'a Type.t

module Make_subtype (M : Make_subtype_arg) = struct
  open M

  type nonrec t = t [@@deriving sexp_of]

  let is_in_subtype = is_in_subtype
  let to_value t = t

  let of_value_exn t =
    if not (is_in_subtype t)
    then
      raise_s
        [%message
          (concat [ "["; name; "]'s [of_value_exn] got value not in subtype" ]) ~_:(t : t)];
    t
  ;;

  let type_ = Type.create [%message name] [%sexp_of: t] of_value_exn to_value
  let eq (t1 : t) t2 = eq t1 t2
end

module Stat = struct
  type t =
    { emacs_free_performed : int
    ; emacs_free_scheduled : int
    }
  [@@deriving sexp_of]

  external num_emacs_free_performed : unit -> int = "ecaml_num_emacs_free_performed"
  external num_emacs_free_scheduled : unit -> int = "ecaml_num_emacs_free_scheduled"

  let now () =
    { emacs_free_performed = num_emacs_free_performed ()
    ; emacs_free_scheduled = num_emacs_free_scheduled ()
    }
  ;;

  let diff t2 t1 =
    { emacs_free_performed = t2.emacs_free_performed - t1.emacs_free_performed
    ; emacs_free_scheduled = t2.emacs_free_scheduled - t1.emacs_free_scheduled
    }
  ;;
end

module Expert = struct
  let have_active_env = have_active_env
  let non_local_exit_signal = non_local_exit_signal
  let raise_if_emacs_signaled = raise_if_emacs_signaled
end

module For_testing = struct
  exception Elisp_signal = Elisp_signal

  let map_elisp_signal g ~f =
    match g () with
    | a -> a
    | exception Elisp_signal { symbol; data } ->
      Nothing.unreachable_code
        (f ~symbol ~data ~reraise:(fun ~symbol ~data ->
           raise (Elisp_signal { symbol; data })))
  ;;

  let map_elisp_signal_omit_data f =
    map_elisp_signal f ~f:(fun ~symbol ~data:_ ~reraise -> reraise ~symbol ~data:nil)
  ;;
end
