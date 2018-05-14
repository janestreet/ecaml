open! Core_kernel
open! Import0

module Q = struct
  include Q
  let apply                            = "apply"                            |> Symbol.intern
end

include Value.Make_subtype (struct
    let name = "function"
    let here = [%here]
    let is_in_subtype = Value.is_function
  end)

module Fn = struct
  type t = Value.t array -> Value.t [@@deriving sexp_of]

  let type_id = Type_equal.Id.create ~name:"Ecaml.Fn" sexp_of_t
  let ecaml_type = Value.Type.caml_embed type_id
end

type 'a with_spec
  =  ?docstring     : string
  -> ?interactive   : string
  -> ?optional_args : Symbol.t list
  -> ?rest_arg      : Symbol.t
  -> Source_code_position.t
  -> args           : Symbol.t list
  -> 'a

module Expert = struct
  let raise_in_dispatch = ref false
end

let create =
  let module M = struct
    (** [make_dispatch_function docstring] returns a primitive Emacs function whose
        documentation is [docstring] and that, when called from Emacs with arguments
        [function_id] and [args], calls [dispatch_function function_id args].

        This is the only emacs function that we create using emacs module C API. All other
        functions are lambdas that call this function. *)
    external make_dispatch_function : string -> Value.t
      = "ecaml_make_dispatch_function"
  end in
  let open M in
  (* [dispatch_function] is registered and emacs [dispatch] function is created before any
     callback is created and can be called *)
  Ecaml_callback.(register dispatch_function)
    ~f:(fun callback_id args ->
      if !Expert.raise_in_dispatch then raise_s [%message "dispatch"];
      try
        let callback = Caml_embed.lookup_by_id_exn callback_id Fn.type_id in
        callback args
      with exn -> Value.Expert.non_local_exit_signal exn; Value.nil);
  let dispatch =
    make_dispatch_function
      ([%message
        "call-OCaml-function"
          ~implemented_at:([%here] : Source_code_position.t)]
       |> Sexp.to_string)
  in
  fun ?docstring ?interactive ?optional_args ?rest_arg here ~args callback ->
    let callback = Fn.ecaml_type.to_value callback in
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
    let module F = Form in
    F.lambda
      ?docstring
      ?interactive
      ?optional_args
      ?rest_arg
      here
      ~args
      ~body:(F.(list
                  ([ symbol Q.apply
                   ; of_value_exn dispatch
                   ; of_value_exn callback
                   ] @
                   (List.map ~f:symbol
                      (args
                       @ ( optional_args |> Option.value ~default:[])
                       @ [ rest_arg      |> Option.value ~default:Q.nil ])))))
    |> F.eval
    |> of_value_exn
;;

module For_testing = struct
  let defun_symbols = ref []
end

let defun ?docstring ?interactive ?optional_args ?rest_arg here ~args symbol f =
  Load_history.add_entry here (Fun symbol);
  For_testing.defun_symbols := symbol :: !For_testing.defun_symbols;
  Symbol.set_function symbol
    (create ?docstring ?interactive ?optional_args ?rest_arg here ~args f
     |> to_value);
;;
