open! Core
open! Import

module Q = struct
  let apply = "apply" |> Symbol.intern
  let interactive = "interactive" |> Symbol.intern
  let lambda = "lambda" |> Symbol.intern
  let list = "list" |> Symbol.intern

  module A = struct
    let optional = "&optional" |> Symbol.intern
    let rest = "&rest" |> Symbol.intern
  end
end

include Value.Make_subtype (struct
    let name = "function"
    and here = [%here]
    and is_in_subtype = Value.is_function
  end)

module Fn = struct
  type t = Value.t array -> Value.t [@@deriving sexp_of]

  let type_id = Type_equal.Id.create ~name:"Ecaml.Fn" sexp_of_t
  let ecaml_type = Caml_embed.create_type type_id
end

module Expert = struct
  let raise_in_dispatch = ref false
end

(** Returns a form which applies [callback] to [args] and [rest_arg]. *)
let call_ecaml_form =
  let module M = struct
    (** [make_dispatch_function docstring] returns a primitive Emacs function whose
        documentation is [docstring] and that, when called from Emacs with arguments
        [function_id] and [args], calls [dispatch_function function_id args].

        This is the only emacs function that we create using emacs module C API. All other
        functions are lambdas that call this function. *)
    external make_dispatch_function : string -> Value.t = "ecaml_make_dispatch_function"
  end
  in
  let open M in
  (* [dispatch_function] is registered and emacs [dispatch] function is created before any
     callback is created and can be called *)
  Ecaml_callback.(register dispatch_function)
    ~should_run_holding_async_lock:true
    ~f:(fun callback_id args ->
      if !Expert.raise_in_dispatch
      then raise_s [%message "Function.Expert.raise_in_dispatch set"];
      try
        let callback = Caml_embed.lookup_by_id_exn callback_id Fn.type_id in
        callback args
      with
      | exn ->
        Value.Expert.non_local_exit_signal exn;
        Value.nil);
  let dispatch =
    make_dispatch_function
      ([%message "call-OCaml-function" ~implemented_at:([%here] : Source_code_position.t)]
       |> Sexp.to_string)
  in
  fun (callback : Fn.t) (args : Form.t list) (rest_arg : Form.t option) ->
    let callback = Value.Type.to_value Fn.ecaml_type callback in
    Form.list
      ([ Form.symbol Q.apply; Form.of_value_exn dispatch; Form.of_value_exn callback ]
       @ args
       @ [ Option.value rest_arg ~default:Form.nil ])
;;

module Interactive = struct
  type t =
    | Args of (unit -> Value.t list Async_kernel.Deferred.t)
    | Form of Form.t
    | Function_name of { prompt : string }
    | Ignored
    | No_arg
    | Prompt of string
    | Raw_prefix
    | Prefix
    | Region

  let to_form = function
    | Args f ->
      call_ecaml_form
        (function
          | [||] -> Value.list (Value.Private.block_on_async f)
          | _ -> assert false)
        []
        None
    | Form form -> form
    | Function_name { prompt } -> sprintf "a%s" prompt |> Form.string
    | Ignored -> "i" |> Form.string
    | No_arg -> "" |> Form.string
    | Prompt prompt -> sprintf "s%s" prompt |> Form.string
    | Raw_prefix -> "P" |> Form.string
    | Prefix -> "p" |> Form.string
    | Region -> "r" |> Form.string
  ;;

  let list vals = Form (Form.apply Q.list (List.map ~f:Form.quote vals))
end

let create here ?docstring ?interactive ~args ?(optional_args = []) ?rest_arg callback =
  (* We wrap [callback] with a lambda expression that, when called, calls [dispatch]
     with the [callback] and the same arguments. This way, lambda expression holds on to
     the [callback] so [callback] is alive as long there is a reference to the lambda
     expression.

     This is a simple way to ensure that [callback] is alive as long as it can be called
     by emacs. Creating a primitive function object (like we do for dispatch) would be
     more efficient but there is no way to attach a reference or a finalizer to that
     kind of object so we use lambda here.

     We do not need to hold on to the lambda expression from OCaml, because Emacs will
     hold on to it. In particular, if the OCaml finalizer for the lambda-expression
     OCaml value runs, that will decrement the Emacs refcount, but will still leave it
     to Emacs to run [callback]'s finalizer once the lambda is not referenced anymore. *)
  let docstring =
    let here =
      concat [ "Implemented at ["; here |> Source_code_position.to_string; "]." ]
    in
    match docstring with
    | None -> here
    | Some s ->
      if String.mem s '\000'
      then raise_s [%message "docstring contains a NUL byte" (s : string)];
      let s = String.capitalize s |> String.strip in
      concat [ (if String.is_empty s then "" else concat [ s; "\n\n" ]); here ]
  in
  [ Form.symbol Q.lambda
  ; Form.list
      (List.map
         ~f:Form.symbol
         (args
          @ (match optional_args with
             | [] -> []
             | optional_args -> Q.A.optional :: optional_args)
          @ Option.value_map rest_arg ~default:[] ~f:(fun rest_arg ->
            [ Q.A.rest; rest_arg ])))
  ; Form.string docstring
  ]
  @ (Option.map interactive ~f:(fun interactive ->
       Form.list [ Form.symbol Q.interactive; Interactive.to_form interactive ])
     |> Option.to_list)
  @ [ call_ecaml_form
        callback
        (List.map ~f:Form.symbol (args @ optional_args))
        (Option.map ~f:Form.symbol rest_arg)
    ]
  |> Form.list
  |> Form.Blocking.eval
  |> of_value_exn
;;

let create_nullary here ?docstring ?interactive f =
  create here ?docstring ?interactive ~args:[] (fun _ ->
    f ();
    Value.nil)
;;

let of_symbol symbol = of_value_exn (Symbol.to_value symbol)
