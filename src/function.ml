open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "function"
    let here = [%here]
    let is_in_subtype = Value.is_function
  end)

module Fn = struct
  type t = Value.t array -> Value.t [@@deriving sexp_of]
end

module Function_table_entry = struct
  type t =
    { callback : Fn.t }
  [@@deriving fields, sexp_of]
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
  let function_table : (Function_id.t, Function_table_entry.t) Hashtbl.t =
    Function_id.Table.create ()
  in
  let module M = struct
    (** [make_function_internal docstring function_id] returns two values:

        - An Emacs function whose documentation is [docstring] and that, when called from
        Emacs with arguments [args], calls [dispatch_function function_id args].

        - An Emacs user_ptr whose finalizer calls [free_function function_id]. *)
    external make_function_internal : string -> Function_id.t -> Value.t * Value.t
      = "ecaml_make_function"
  end in
  let open M in
  Ecaml_callback.(register free_function)
    ~f:(fun function_id ->
      if debug
      then eprint_s [%message "Function.free_function" (function_id : Function_id.t)];
      Hashtbl.remove function_table function_id);
  (* The registration of [dispatch_function] happens before any callback from Emacs to
     OCaml.  We ensure this because [ecaml_make_function] is the only C code that can
     cause [dispatch_function] to be called. *)
  Ecaml_callback.(register dispatch_function)
    ~f:(fun function_id args ->
      if !Expert.raise_in_dispatch then raise_s [%message "dispatch"];
      try
        let { Function_table_entry. callback } =
          Hashtbl.find_exn function_table function_id in
        callback args
      with exn -> Value.Expert.non_local_exit_signal exn; Value.nil);
  fun ?docstring ?interactive ?optional_args ?rest_arg here ~args callback ->
    let function_id = Function_id.create () in
    Hashtbl.set function_table ~key:function_id ~data:{ callback };
    let emacs_function, sentinel =
      make_function_internal
        ([%message
          "call-OCaml-function"
            (function_id : Function_id.t)
            ~implemented_at:(here : Source_code_position.t)]
         |> Sexp.to_string)
        function_id
    in
    (* We wrap [emacs_function] with a lambda expression that, when called, calls
       [emacs_function] with the same arguments.  The lambda expression also points to
       [sentinel], so that when the lambda expression is collected, we run the
       [sentinel]'s Emacs finalizer, which calls [free_function], which removes
       [function_id] from [function_table].  We do not need to hold on to the lambda
       expression from OCaml, because Emacs will hold on to it.  In particular, if the
       OCaml finalizer for the lambda-expression OCaml value runs, that will decrement the
       Emacs refcount, but will still leave it to Emacs to run [sentinel]'s finalizer
       whenever the lambda expression is collected. *)
    let module F = Form in
    F.lambda
      ?docstring
      ?interactive
      ?optional_args
      ?rest_arg
      here
      ~args
      ~body:(F.progn
               [ sentinel |> F.quote
               ; F.list
                   ([ Q.apply |> F.symbol
                    ; emacs_function |> F.quote ]
                    @ (List.map ~f:F.symbol
                         (args
                          @ ( optional_args |> Option.value ~default:[])
                          @ [ rest_arg      |> Option.value ~default:Q.nil ])))])
    |> F.eval
    |> of_value_exn
;;

let defun ?docstring ?interactive ?optional_args ?rest_arg here ~args symbol f =
  Load_history.add_entry here (Fun symbol);
  Symbol.set_function symbol
    (create ?docstring ?interactive ?optional_args ?rest_arg here ~args f
     |> to_value);
;;
