open! Core_kernel
open! Async_kernel
open! Import
include Defun_intf

module Q = struct
  let defalias = "defalias" |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let defalias =
    Q.defalias <: Symbol.type_ @-> Symbol.type_ @-> option string @-> return nil
  ;;
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

  let required name type_ = Required (name, type_)
  let optional name type_ = Optional (name, Value.Type.option type_)
  let rest name type_ = Rest (name, type_)

  let optional_with_nil name type_ =
    map (optional name type_) ~f:(function
      | None -> type_.of_value_exn Value.nil
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

let apply t args =
  let len = Array.length args in
  let pos = ref 0 in
  let consume_arg (type_ : _ Value.Type.t) =
    let arg = type_.of_value_exn args.(!pos) in
    incr pos;
    arg
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
      | Required (_, type_) ->
        if Int.( >= ) !pos len
        then
          raise_s
            [%message
              "Not enough arguments.  Emacs should have raised wrong-number-of-arguments."]
        else consume_arg type_
      | Optional (_, type_) -> if Int.( >= ) !pos len then None else consume_arg type_
      | Rest (_, type_) ->
        let retval =
          Array.sub args ~pos:!pos ~len:(len - !pos)
          |> Array.map ~f:type_.of_value_exn
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
  type 'a t =
    | Returns of 'a Value.Type.t
    | Returns_unit_deferred : unit Deferred.t t
  [@@deriving sexp_of]
end

let block_on_async
  :
    (Source_code_position.t
     -> ?context:Sexp.t Lazy.t
     -> (unit -> unit Deferred.t)
     -> unit)
      Set_once.t =
  Set_once.create ()
;;

let call (type a) (t : a t) here ~function_ ~args ~(returns : a Returns.t) =
  match returns with
  | Returns returns -> returns.to_value (apply t args)
  | Returns_unit_deferred ->
    let block_on_async = Set_once.get_exn block_on_async [%here] in
    block_on_async
      here
      ~context:
        (lazy
          (List (function_ :: (args |> Array.map ~f:Value.sexp_of_t |> Array.to_list))))
      (fun () -> apply t args);
    Value.nil
;;

module Interactive = struct
  type t =
    | No_arg
    | Ignored
    | Prompt of string
    | Raw_prefix
    | Region

  let to_string = function
    | No_arg -> ""
    | Ignored -> "i"
    | Prompt prompt -> sprintf "s%s" prompt
    | Raw_prefix -> "P"
    | Region -> "r"
  ;;

  let ({ Value.Type.of_value_exn; to_value; _ } as type_) =
    Value.Type.(
      map
        string
        ~name:[%message "interactive-code"]
        ~of_:(function
          | "" -> No_arg
          | "i" -> Ignored
          | "P" -> Raw_prefix
          | "r" -> Region
          | s ->
            (match String.chop_prefix s ~prefix:"s" with
             | Some prompt -> Prompt prompt
             | None -> raise_s [%sexp "Unimplemented interactive code", (s : string)]))
        ~to_:to_string)
  ;;
end

module For_testing = struct
  let defun_symbols = ref []
end

let add_to_load_history symbol here =
  Load_history.add_entry here (Fun symbol);
  For_testing.defun_symbols := symbol :: !For_testing.defun_symbols
;;

let defalias symbol here ?docstring ~alias_of () =
  add_to_load_history symbol here;
  F.defalias symbol alias_of (docstring |> Option.map ~f:String.strip)
;;

let define_obsolete_alias obsolete here ?docstring ~alias_of ~since () =
  defalias obsolete here ?docstring ~alias_of ();
  Obsolete.make_function_obsolete obsolete ~current:alias_of ~since
;;

let defun_raw symbol here ?docstring ?interactive ~args ?optional_args ?rest_arg f =
  add_to_load_history symbol here;
  Symbol.set_function
    symbol
    (Function.create here ?docstring ?interactive ~args ?optional_args ?rest_arg f
     |> Function.to_value)
;;

let defun_internal
      symbol
      here
      ?docstring
      ?(define_keys = [])
      ?obsoletes
      ?interactive
      t
      fn
  =
  let args = get_args t in
  defun_raw
    symbol
    here
    ?docstring
    ?interactive:(Option.map interactive ~f:Interactive.to_string)
    ~args:args.required
    ~optional_args:args.optional
    ?rest_arg:args.rest
    fn;
  List.iter define_keys ~f:(fun (keymap, keys) ->
    Keymap.define_key keymap (Key_sequence.create_exn keys) (Symbol symbol));
  Option.iter obsoletes ~f:(fun obsolete ->
    define_obsolete_alias obsolete here ~alias_of:symbol ~since:"who knows when" ())
;;

let defun symbol here ?docstring ?define_keys ?obsoletes ?interactive returns t =
  let function_ = [%sexp (symbol : Symbol.t)] in
  defun_internal
    ?docstring
    ?define_keys
    ?obsoletes
    ?interactive
    symbol
    here
    t
    (fun args -> call t here ~function_ ~args ~returns)
;;

let defun_nullary symbol here ?docstring ?define_keys ?obsoletes ?interactive returns f =
  defun
    symbol
    here
    ?docstring
    ?define_keys
    ?obsoletes
    ?interactive
    returns
    (let open Let_syntax in
     let%map_open () = return () in
     f ())
;;

let defun_nullary_nil symbol here ?docstring ?define_keys ?obsoletes ?interactive f =
  defun_nullary
    symbol
    here
    ?docstring
    ?define_keys
    ?obsoletes
    ?interactive
    (Returns Value.Type.unit)
    f
;;

let lambda here ?docstring ?interactive returns t =
  let function_ = [%message "Defun.lambda"] in
  let args = get_args t in
  Function.create
    here
    ?docstring
    ?interactive:(Option.map interactive ~f:Interactive.to_string)
    ~optional_args:args.optional
    ?rest_arg:args.rest
    ~args:args.required
    (fun args -> call t here ~function_ ~args ~returns)
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

module Private = struct
  let block_on_async = block_on_async
end
