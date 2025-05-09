open! Core
open! Async_kernel
open! Import
include Defun_intf

module Q = struct
  include Q

  module A = struct
    let optional = "&optional" |> Symbol.intern
    let rest = "&rest" |> Symbol.intern
  end
end

module T0 = struct
  type _ t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Required : Symbol.t * 'a Value.Type.t -> 'a t
    | Optional : Symbol.t * 'a option Value.Type.t -> 'a option t
    | Rest : Symbol.t * 'a Value.Type.t -> 'a list t

  let rec sexp_of_t : type a. (a -> Sexp.t) -> a t -> Sexp.t =
    fun sexp_of_a -> function
    | Return a -> [%message "Return" ~_:(a : a)]
    | Map (t, _) -> [%message "Map" ~_:(t : _ t)]
    | Both (t1, t2) -> [%message "Both" ~_:(t1 : _ t) ~_:(t2 : _ t)]
    | Required (symbol, type_) ->
      [%message "Required" ~_:(symbol : Symbol.t) ~_:(type_ : a Value.Type.t)]
    | Optional (symbol, type_) ->
      [%message "Optional" ~_:(symbol : Symbol.t) ~_:(type_ : a Value.Type.t)]
    | Rest (symbol, type_) ->
      [%message "Rest" ~_:(symbol : Symbol.t) ~_:(type_ : _ Value.Type.t)]
  ;;

  let return x = Return x
  let map t ~f = Map (t, f)
  let both t t' = Both (t, t')
  let apply f x = both f x |> map ~f:(fun (f, x) -> f x)
  let map = `Custom map
end

module T = struct
  include T0
  include Applicative.Make (T0)

  let required name type_ = Required (name |> Symbol.intern, type_)
  let optional name type_ = Optional (name |> Symbol.intern, Value.Type.nil_or type_)
  let rest name type_ = Rest (name |> Symbol.intern, type_)

  let optional_with_nil name type_ =
    map (optional name type_) ~f:(function
      | None -> Value.Type.of_value_exn type_ Value.nil
      | Some x -> x)
  ;;

  let optional_with_default name default type_ =
    map (optional name type_) ~f:(Option.value ~default)
  ;;

  include (Value.Type : Value.Type.S)
end

include T

module Open_on_rhs_intf = struct
  module type S = S with type 'a t = 'a t
end

include Applicative.Make_let_syntax (T) (Open_on_rhs_intf) (T)

let apply t args ~function_ ~defined_at =
  let len = Array.length args in
  let pos = ref 0 in
  let convert_arg arg type_ value =
    try Value.Type.of_value_exn type_ value with
    | exn ->
      raise_s
        [%message
          "function got argument of wrong type"
            ~_:(function_ : Sexp.t)
            (defined_at : Source_code_position.t)
            (arg : Symbol.t)
            ~_:(exn : exn)]
  in
  let consume_arg symbol type_ =
    let a = convert_arg symbol type_ args.(!pos) in
    incr pos;
    a
  in
  let rec loop : type a. a t -> a =
    fun t ->
    match t with
    | Return a -> a
    | Map (t, f) -> f (loop t)
    | Both (t1, t2) ->
      let x1 = loop t1 in
      let x2 = loop t2 in
      x1, x2
    | Required (symbol, type_) ->
      if Int.( >= ) !pos len
      then
        raise_s
          [%message
            "Not enough arguments.  Emacs should have raised wrong-number-of-arguments."]
      else consume_arg symbol type_
    | Optional (symbol, type_) ->
      if Int.( >= ) !pos len then None else consume_arg symbol type_
    | Rest (symbol, type_) ->
      let retval =
        Array.sub args ~pos:!pos ~len:(len - !pos)
        |> Array.map ~f:(fun value -> convert_arg symbol type_ value)
        |> Array.to_list
      in
      pos := len;
      retval
  in
  let result = loop t in
  let pos = !pos in
  if Int.( <> ) pos len
  then
    raise_s
      [%message
        "Extra arguments.  Emacs should have raised wrong-number-of-arguments."
          ~used:(pos : int)
          (args : Value.t array)];
  result
;;

module Args = struct
  type t =
    { required : Symbol.t list
    ; optional : Symbol.t list
    ; rest : Symbol.t option
    }

  let sexp_of_t { required; optional; rest } =
    [%sexp
      (List.concat
         [ required
         ; (if List.is_empty optional then [] else Q.A.optional :: optional)
         ; (match rest with
            | None -> []
            | Some rest -> [ Q.A.rest; rest ])
         ]
       : Symbol.t list)]
  ;;

  let empty = { required = []; optional = []; rest = None }
  let add_required t s = { t with required = s :: t.required }
  let add_optional t s = { t with optional = s :: t.optional }

  let add_rest t s =
    match t.rest with
    | None -> { t with rest = Some s }
    | Some rest ->
      raise_s [%message "Multiple rest arguments" ~_:([ rest; s ] : Symbol.t list)]
  ;;
end

let get_args t =
  let rec loop : type a. a t -> _ -> _ =
    fun t args ->
    match t with
    | Return _ -> args
    | Map (t, _) -> loop t args
    | Both (t1, t2) -> loop t1 (loop t2 args)
    | Required (name, _) -> Args.add_required args name
    | Optional (name, _) -> Args.add_optional args name
    | Rest (name, _) -> Args.add_rest args name
  in
  loop t Args.empty
;;

module Returns = struct
  type (_, _) t =
    | Returns : 'a Value.Type.t -> ('a, 'a) t
    | Returns_deferred : 'a Value.Type.t -> ('a, 'a Deferred.t) t

  let sexp_of_t (type a b) _ _ (t : (a, b) t) =
    match t with
    | Returns type_ -> [%message "Returns" (type_ : _ Value.Type.t)]
    | Returns_deferred type_ -> [%message "Returns_deferred" (type_ : _ Value.Type.t)]
  ;;

  let return (type a b) (t : (a, b) t) (a : a) : b =
    match t with
    | Returns _ -> a
    | Returns_deferred _ -> Deferred.return a
  ;;

  let returns (type a b) (sync_or_async : (a, b) Sync_or_async.t) (type_ : a Value.Type.t)
    : (a, b) t
    =
    match sync_or_async with
    | Sync -> Returns type_
    | Async -> Returns_deferred type_
  ;;
end

let call
  (type a b)
  (t : a t)
  here
  ~function_
  ~args
  ~(returns : (b, a) Returns.t)
  ~should_profile
  =
  let should_profile = Option.value should_profile ~default:true in
  let context : Sexp.t Lazy.t =
    lazy (List (function_ :: (args |> Array.map ~f:Value.sexp_of_t |> Array.to_list)))
  in
  let apply () = apply t args ~function_ ~defined_at:here in
  let doit () =
    match returns with
    | Returns returns -> Value.Type.to_value returns (apply ())
    | Returns_deferred returns ->
      Value.Private.block_on_async ~here apply ~context |> Value.Type.to_value returns
  in
  if should_profile then profile Sync context doit else doit ()
;;

module For_testing = struct
  let defun_symbols = ref []
  let all_defun_symbols () = !defun_symbols |> List.sort ~compare:Symbol.compare_name
end

let add_to_load_history symbol here =
  Load_history.add_entry here (Fun symbol);
  For_testing.defun_symbols := symbol :: !For_testing.defun_symbols
;;

let defalias =
  Funcall.Wrap.("defalias" <: Symbol.t @-> value @-> nil_or string @-> return nil)
;;

(* We use defalias for all function definitions, instead of fset.  The former correctly
   handles things like advice that has already been defined for the symbol. *)
let defalias symbol ~(here : [%call_pos]) ?docstring ~alias_of () =
  add_to_load_history symbol here;
  defalias symbol alias_of (docstring |> Option.map ~f:String.strip)
;;

let define_obsolete_alias obsolete ~(here : [%call_pos]) ?docstring ~alias_of ~since () =
  defalias obsolete ~here ?docstring ~alias_of:(alias_of |> Symbol.to_value) ();
  Obsolete.make_function_obsolete obsolete ~current:(Some alias_of) ~since
;;

let defun_raw symbol here ~docstring ?interactive ~args ?optional_args ?rest_arg f =
  let docstring = String.strip docstring in
  require_nonempty_docstring here ~docstring;
  defalias
    symbol
    ~here
    ~alias_of:
      (Function.create here ~docstring ?interactive ~args ?optional_args ?rest_arg f
       |> Function.to_value)
    ()
;;

let maybe_disable_function name disabled =
  (* the user may have explicitly put [disabled nil] for the symbol to undisable the
     function before we define it. So we check for that first *)
  let open Symbol.Property in
  match get function_disabled name with
  | Some _ -> ()
  | None -> put function_disabled name disabled
;;

let defun_internal
  symbol
  here
  ~docstring
  ?(define_keys = [])
  ?(obsoletes : Obsoletes.t option)
  ?interactive
  ?(disabled = Symbol.Disabled.Not_disabled)
  t
  fn
  =
  let symbol, obsoletes =
    if is_some obsoletes
    then symbol, obsoletes
    else (
      match Symbol.Automatic_migration.migrate ~old:symbol with
      | None -> symbol, obsoletes
      | Some { new_; since } -> new_, Some (symbol, Since since))
  in
  let args = get_args t in
  defun_raw
    symbol
    here
    ~docstring
    ?interactive
    ~args:args.required
    ~optional_args:args.optional
    ?rest_arg:args.rest
    fn;
  List.iter define_keys ~f:(fun (keymap, keys) -> Keymap.set keymap keys (Symbol symbol));
  Option.iter obsoletes ~f:(fun (obsolete, Since since) ->
    define_obsolete_alias obsolete ~here ~alias_of:symbol ~since ());
  maybe_disable_function symbol disabled
;;

let defun
  symbol
  here
  ~docstring
  ?define_keys
  ?obsoletes
  ?should_profile
  ?interactive
  ?disabled
  ?evil_config
  returns
  t
  =
  let function_ = [%sexp (symbol : Symbol.t)] in
  defun_internal
    ~docstring
    ?define_keys
    ?obsoletes
    ?interactive
    ?disabled
    symbol
    here
    t
    (fun args -> call t here ~function_ ~args ~returns ~should_profile);
  Option.iter evil_config ~f:(fun evil_config ->
    Evil.Config.apply_to_defun evil_config symbol)
;;

let defun_nullary
  symbol
  here
  ~docstring
  ?define_keys
  ?obsoletes
  ?should_profile
  ?interactive
  ?disabled
  ?evil_config
  returns
  f
  =
  defun
    symbol
    here
    ~docstring
    ?define_keys
    ?obsoletes
    ?should_profile
    ?interactive
    ?disabled
    ?evil_config
    returns
    (let open Let_syntax in
     let%map_open () = return () in
     f ())
;;

let defun_nullary_nil
  symbol
  here
  ~docstring
  ?define_keys
  ?obsoletes
  ?should_profile
  ?interactive
  ?disabled
  ?evil_config
  f
  =
  defun_nullary
    symbol
    here
    ~docstring
    ?define_keys
    ?obsoletes
    ?should_profile
    ?interactive
    ?disabled
    ?evil_config
    (Returns Value.Type.unit)
    f
;;

let lambda here ?docstring ?interactive returns t =
  let args = get_args t in
  let function_ =
    [%message "lambda" ~_:(args : Args.t) ~created_at:(here : Source_code_position.t)]
  in
  Function.create
    here
    ?docstring
    ?interactive
    ~optional_args:args.optional
    ?rest_arg:args.rest
    ~args:args.required
    (fun args -> call t here ~function_ ~args ~returns ~should_profile:None)
;;

let lambda_nullary here ?docstring ?interactive returns f =
  lambda
    here
    ?docstring
    ?interactive
    returns
    (let open Let_syntax in
     let%map_open () = return () in
     f ())
;;

let lambda_nullary_nil here ?docstring ?interactive f =
  lambda_nullary here ?docstring ?interactive (Returns Value.Type.unit) f
;;
