open! Core_kernel
open! Import
open! Async_kernel
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

  let () =
    assert (Variants.return.rank = 0);
    assert (Variants.signal.rank = 1);
    assert (Variants.throw.rank = 2)
  ;;
end

external have_active_env : unit -> bool = "ecaml_have_active_env"

external non_local_exit_get_and_clear
  :  unit
  -> Funcall_exit.t
  = "ecaml_non_local_exit_get_and_clear"

exception
  Elisp_signal of
    { symbol : t
    ; data : t
    }

exception
  Elisp_throw of
    { tag : t
    ; value : t
    }

let raise_if_emacs_signaled () =
  match non_local_exit_get_and_clear () with
  | Return -> ()
  | Signal (symbol, data) -> raise (Elisp_signal { symbol; data })
  | Throw (tag, value) -> raise (Elisp_throw { tag; value })
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
let interned_symbols = String.Hash_set.create ()

let all_interned_symbols () =
  List.sort (Hash_set.to_list interned_symbols) ~compare:[%compare: string]
;;

let intern string =
  if am_running_test then Hash_set.add interned_symbols string;
  intern string
;;

module Q = struct
  let append = "append" |> intern
  let arrayp = "arrayp" |> intern
  let backtrace = "backtrace" |> intern
  let bufferp = "bufferp" |> intern
  let car = "car" |> intern
  let cadr = "cadr" |> intern
  let cdr = "cdr" |> intern
  let commandp = "commandp" |> intern
  let cons = "cons" |> intern
  let consp = "consp" |> intern
  let debug_on_error = "debug-on-error" |> intern
  let equal = "equal" |> intern
  let equal_including_properties = "equal-including-properties" |> intern
  let error = "error" |> intern
  let eventp = "eventp" |> intern
  let floatp = "floatp" |> intern
  let fontp = "fontp" |> intern
  let framep = "framep" |> intern
  let functionp = "functionp" |> intern
  let hash_table_p = "hash-table-p" |> intern
  let keymapp = "keymapp" |> intern
  let length = "length" |> intern
  let list = "list" |> intern
  let markerp = "markerp" |> intern
  let message = "message" |> intern
  let nil = "nil" |> intern
  let prin1_to_string = "prin1-to-string" |> intern
  let print_length = "print-length" |> intern
  let print_level = "print-level" |> intern
  let processp = "processp" |> intern
  let set = "set" |> intern
  let stringp = "stringp" |> intern
  let substring_no_properties = "substring-no-properties" |> intern
  let symbol_value = "symbol-value" |> intern
  let symbolp = "symbolp" |> intern
  let syntax_table_p = "syntax-table-p" |> intern
  let t = "t" |> intern
  let timerp = "timerp" |> intern
  let vector = "vector" |> intern
  let vectorp = "vectorp" |> intern
  let window_configuration_p = "window-configuration-p" |> intern
  let windowp = "windowp" |> intern
end

module Block_on_async = struct
  type t =
    { f :
        'a. Source_code_position.t -> ?context:Sexp.t Lazy.t -> (unit -> 'a Deferred.t)
        -> 'a
    }

  let set_once : t Set_once.t = Set_once.create ()
end

module Enqueue_foreground_block_on_async = struct
  type t =
    { f :
        Source_code_position.t
        -> ?context:Sexp.t Lazy.t
        -> ?raise_exceptions_to_monitor:Monitor.t
        -> (unit -> unit Deferred.t)
        -> unit
    }

  let set_once : t Set_once.t = Set_once.create ()
end

module Run_outside_async = struct
  type t =
    { f :
        'a. Source_code_position.t -> ?allowed_in_background:bool -> (unit -> 'a)
        -> 'a Deferred.t
    }

  let set_once : t Set_once.t = Set_once.create ()
end

let maybe_profile ~should_profile context f =
  if Option.value should_profile ~default:true then profile Sync context f else f ()
;;

external funcall_array : t -> t array -> bool -> t = "ecaml_funcall_array"

let funcall_array ?should_profile t ts ~should_return_result =
  maybe_profile
    ~should_profile
    (lazy [%sexp (t :: (ts |> Array.to_list) : t list)])
    (fun () -> funcall_array t ts should_return_result)
;;

let funcallN_array ?should_profile t ts =
  let r = funcall_array ?should_profile t ts ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcallN_array_i ?should_profile t ts =
  ignore (funcall_array ?should_profile t ts ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcallN ?should_profile t ts = funcallN_array ?should_profile t (ts |> Array.of_list)

let funcallN_i ?should_profile t ts =
  funcallN_array_i ?should_profile t (ts |> Array.of_list)
;;

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

let external_funcall1 = funcall1

let funcall0 f ~should_profile ~should_return_result =
  maybe_profile
    ~should_profile
    (lazy [%sexp ([ f ] : t list)])
    (fun () -> funcall0 f should_return_result)
;;

let funcall1 f a1 ~should_profile ~should_return_result =
  maybe_profile
    ~should_profile
    (lazy [%sexp ([ f; a1 ] : t list)])
    (fun () -> funcall1 f a1 should_return_result)
;;

let funcall2 f a1 a2 ~should_profile ~should_return_result =
  maybe_profile
    ~should_profile
    (lazy [%sexp ([ f; a1; a2 ] : t list)])
    (fun () -> funcall2 f a1 a2 should_return_result)
;;

let funcall3 f a1 a2 a3 ~should_profile ~should_return_result =
  maybe_profile
    ~should_profile
    (lazy [%sexp ([ f; a1; a2; a3 ] : t list)])
    (fun () -> funcall3 f a1 a2 a3 should_return_result)
;;

let funcall4 f a1 a2 a3 a4 ~should_profile ~should_return_result =
  maybe_profile
    ~should_profile
    (lazy [%sexp ([ f; a1; a2; a3; a4 ] : t list)])
    (fun () -> funcall4 f a1 a2 a3 a4 should_return_result)
;;

let funcall5 f a1 a2 a3 a4 a5 ~should_profile ~should_return_result =
  maybe_profile
    ~should_profile
    (lazy [%sexp ([ f; a1; a2; a3; a4; a5 ] : t list)])
    (fun () -> funcall5 f a1 a2 a3 a4 a5 should_return_result)
;;

let funcall0_i ?should_profile f =
  ignore (funcall0 f ~should_profile ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall0 ?should_profile f =
  let r = funcall0 f ~should_profile ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall1_i ?should_profile f a =
  ignore (funcall1 f a ~should_profile ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall1 ?should_profile f a =
  let r = funcall1 f a ~should_profile ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall2_i ?should_profile f a1 a2 =
  ignore (funcall2 f a1 a2 ~should_profile ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall2 ?should_profile f a1 a2 =
  let r = funcall2 f a1 a2 ~should_profile ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall3_i ?should_profile f a1 a2 a3 =
  ignore (funcall3 f a1 a2 a3 ~should_profile ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall3 ?should_profile f a1 a2 a3 =
  let r = funcall3 f a1 a2 a3 ~should_profile ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall4_i ?should_profile f a1 a2 a3 a4 =
  ignore (funcall4 f a1 a2 a3 a4 ~should_profile ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall4 ?should_profile f a1 a2 a3 a4 =
  let r = funcall4 f a1 a2 a3 a4 ~should_profile ~should_return_result:true in
  raise_if_emacs_signaled ();
  r
;;

let funcall5_i ?should_profile f a1 a2 a3 a4 a5 =
  ignore (funcall5 f a1 a2 a3 a4 a5 ~should_profile ~should_return_result:false : t);
  raise_if_emacs_signaled ()
;;

let funcall5 ?should_profile f a1 a2 a3 a4 a5 =
  let r = funcall5 f a1 a2 a3 a4 a5 ~should_profile ~should_return_result:true in
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

let funcall_int_int_value_unit ?should_profile f a1 a2 a3 =
  maybe_profile
    ~should_profile
    (lazy [%sexp (f : t), (a1 : int), (a2 : int), (a3 : t)])
    (fun () -> funcall_int_int_value_unit f a1 a2 a3);
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

let funcall_int_int_value_value_unit ?should_profile f a1 a2 a3 a4 =
  maybe_profile
    ~should_profile
    (lazy [%sexp (f : t), (a1 : int), (a2 : int), (a3 : t), (a4 : t)])
    (fun () -> funcall_int_int_value_value_unit f a1 a2 a3 a4);
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

let get_var symbol = funcall1 Q.symbol_value symbol
let set_var symbol value = funcall2_i Q.set symbol value
let get_int_var string = get_var (string |> intern) |> to_int_exn
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
  Hashtbl.find_or_add of_utf8_bytes_cache string ~default:(fun () ->
    of_utf8_bytes string)
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

let message_zero_alloc t =
  ignore (external_funcall1 Q.message t false : t);
  raise_if_emacs_signaled ()
;;

let message_t t = funcall2_i Q.message percent_s t
let messagef fmt = ksprintf message fmt

let message_s : Sexp.t -> unit = function
  | Atom string -> message string
  | sexp -> messagef "%s" (sexp |> Sexp.to_string_hum)
;;

let nil = Q.nil
let t = Q.t
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

let list_to_array_exn t ~f =
  let length = funcall1 Q.length t |> to_int_exn in
  if length = 0
  then Array.of_list []
  else (
    let elt0 = f (car_exn t) in
    let array = Array.create ~len:length elt0 in
    let rec fill_array_loop t i =
      if is_nil t
      then ()
      else (
        array.(i) <- f (car_exn t);
        fill_array_loop (cdr_exn t) (i + 1))
    in
    fill_array_loop (cdr_exn t) 1;
    array)
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

    (** [non_local_exit_throw] works like [non_local_exit_signal], except that it throws
        instead of signaling when our code returns to emacs. *)
    external non_local_exit_throw : t -> t -> unit = "ecaml_non_local_exit_throw"
  end
  in
  let debug_on_error = debug_on_error () in
  match exn with
  | Elisp_throw { tag; value } -> M.non_local_exit_throw tag value
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
    M.non_local_exit_signal symbol data
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
    M.non_local_exit_signal Q.error (list [ message |> of_utf8_bytes ])
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

let with_print_config ~print_length ~print_level ~f =
  let old_print_length = get_var Q.print_length in
  let old_print_level = get_var Q.print_level in
  set_var Q.print_length (print_length |> of_int_exn);
  set_var Q.print_level (print_level |> of_int_exn);
  protect ~f ~finally:(fun () ->
    set_var Q.print_length old_print_length;
    set_var Q.print_level old_print_level)
;;

let rec sexp_of_t_internal t ~print_length ~print_level : Sexp.t =
  if print_length <= 0 || print_level <= 0
  then [%message "..."]
  else if is_string t && not (text_has_properties t)
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
    let car =
      sexp_of_t_internal (car_exn t) ~print_length ~print_level:(print_level - 1)
    in
    let cdr =
      sexp_of_t_internal (cdr_exn t) ~print_length:(print_length - 1) ~print_level
    in
    match cdr with
    | Atom "nil" -> List [ car ]
    | Atom _ -> List [ car; Atom "."; cdr ]
    | List sexps -> List (car :: sexps))
  else (
    let sexp_string =
      with_print_config ~print_length ~print_level ~f:(fun () -> prin1_to_string t)
    in
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

let ecaml_profile_print_length = ref None
let ecaml_profile_print_level = ref None

let sexp_of_t t =
  let print_length, print_level =
    if Profile.am_forcing_message ()
    then !ecaml_profile_print_length, !ecaml_profile_print_level
    else (
      let int_or_nil symbol =
        let value = get_var symbol in
        if is_nil value then None else Some (value |> to_int_exn)
      in
      int_or_nil Q.print_length, int_or_nil Q.print_level)
  in
  let print_length = Option.value print_length ~default:emacs_max_int in
  let print_level = Option.value print_level ~default:emacs_max_int in
  sexp_of_t_internal t ~print_length ~print_level
;;

let () =
  Sexplib.Conv.Exn_converter.add [%extension_constructor Elisp_signal] (function
    | Elisp_signal { symbol; data } ->
      if eq symbol Q.error
      then [%sexp (data : t)]
      else if is_nil data
      then [%sexp (symbol : t)]
      else [%message "" ~_:(symbol : t) ~_:(data : t)]
    | _ ->
      (* Reaching this branch indicates a bug in sexplib. *)
      assert false);
  sexp_of_t_ref := sexp_of_t
;;

module Type = struct
  type async [@@deriving sexp_of]
  type sync [@@deriving sexp_of]

  type ('a, 'm) t_ =
    { id : 'a Type_equal.Id.t
    ; of_value_exn : value -> 'a
    ; to_value : 'a -> value
    }
  [@@deriving fields, sexp_of]

  type 'a t = ('a, sync) t_ [@@deriving sexp_of]

  module type S = Type with type value := value with type 'a t := 'a t

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
         of_value_exn t1 (car_exn cons_cell), of_value_exn t2 (cdr_exn cons_cell)))
      (fun l ->
         list (List.map l ~f:(fun (a, b) -> cons (to_value t1 a) (to_value t2 b))))
  ;;

  let list t =
    create
      [%message "list" ~_:(name t : Sexp.t)]
      (sexp_of_list (to_sexp t))
      (to_list_exn ~f:(of_value_exn t))
      (fun l -> list (List.map l ~f:(to_value t)))
  ;;

  let array_as_list t =
    create
      [%message "array-as-list" ~_:(name t : Sexp.t)]
      (sexp_of_array (to_sexp t))
      (list_to_array_exn ~f:(of_value_exn t))
      (Array.fold_right ~init:nil ~f:(fun elt elisp_list ->
         cons (to_value t elt) elisp_list))
  ;;

  let vector t =
    create
      [%message "vector" ~_:(name t : Sexp.t)]
      (sexp_of_array (to_sexp t))
      (to_array_exn ~f:(of_value_exn t))
      (fun a -> vector (Array.map a ~f:(to_value t)))
  ;;

  let tuple t1 t2 =
    create
      [%sexp (name t1 : Sexp.t), (name t2 : Sexp.t)]
      (Tuple2.sexp_of_t (to_sexp t1) (to_sexp t2))
      (fun v -> of_value_exn t1 (car_exn v), of_value_exn t2 (cdr_exn v))
      (fun (a1, a2) -> cons (to_value t1 a1) (to_value t2 a2))
  ;;

  let tuple2_as_list t1 t2 =
    create
      [%sexp (name t1 : Sexp.t), (name t2 : Sexp.t)]
      (Tuple2.sexp_of_t (to_sexp t1) (to_sexp t2))
      (fun v -> of_value_exn t1 (car_exn v), of_value_exn t2 (cadr_exn v))
      (fun (a1, a2) -> cons (to_value t1 a1) (cons (to_value t2 a2) nil))
  ;;

  let map t ~name ~of_ ~to_ =
    create
      name
      (Fn.compose (to_sexp t) to_)
      (Fn.compose of_ (of_value_exn t))
      (Fn.compose (to_value t) to_)
  ;;

  let map_id t name = map t ~name ~of_:Fn.id ~to_:Fn.id

  let nil_or t =
    create
      [%message "nil_or" ~_:(name t : Sexp.t)]
      (function
        | None -> Atom "nil"
        | Some v -> to_sexp t v)
      (fun v -> if is_nil v then None else Some (v |> of_value_exn t))
      (function
        | None -> nil
        | Some v -> (to_value t) v)
  ;;

  let option t =
    create
      [%message "option" ~_:(name t : Sexp.t)]
      (sexp_of_option (to_sexp t))
      (fun v -> if is_nil v then None else Some (car_exn v |> of_value_exn t))
      (function
        | None -> nil
        | Some v -> cons (to_value t v) nil)
  ;;

  (* embed ocaml values as strings which are sexp representations *)
  let sexpable (type a) (module A : Sexpable with type t = a) ~name =
    create
      [%message "sexpable" ~_:(name : Sexp.t)]
      [%sexp_of: A.t]
      (fun v -> A.t_of_sexp (Sexp.of_string (of_value_exn string v)))
      (fun a -> A.sexp_of_t a |> Sexp.to_string_mach |> to_value string)
  ;;

  let path_list =
    map
      (nil_or string)
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
          (concat [ "["; name; "]'s [of_value_exn] got value not in subtype" ])
            ~_:(t : t)];
    t
  ;;

  let type_ = Type.create [%message name] [%sexp_of: t] of_value_exn to_value
  let t = type_
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
  let all_interned_symbols = all_interned_symbols

  exception Elisp_signal = Elisp_signal
  exception Elisp_throw = Elisp_throw

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

module Private = struct
  module Block_on_async = Block_on_async
  module Enqueue_foreground_block_on_async = Enqueue_foreground_block_on_async
  module Run_outside_async = Run_outside_async

  let block_on_async here ?context f =
    (Set_once.get_exn Block_on_async.set_once here).f here ?context f
  ;;

  let enqueue_foreground_block_on_async here ?context ?raise_exceptions_to_monitor f =
    (Set_once.get_exn Enqueue_foreground_block_on_async.set_once here).f
      here
      ?context
      ?raise_exceptions_to_monitor
      f
  ;;

  let run_outside_async here ?allowed_in_background f =
    (Set_once.get_exn Run_outside_async.set_once here).f here ?allowed_in_background f
  ;;

  let ecaml_profile_print_length = ecaml_profile_print_length
  let ecaml_profile_print_level = ecaml_profile_print_level
  let message_zero_alloc = message_zero_alloc
  let message_t = message_t
end
