open! Core_kernel
open! Import

module Class = struct
  type t =
    | After
    | Around
    | Before
  [@@deriving sexp_of]

  let to_symbol = function
    | After  -> Q.after
    | Around -> Q.around
    | Before -> Q.before
  ;;

  let to_value t = t |> to_symbol |> Symbol.to_value
end

module Position = struct
  type t =
    | First
    | Last
    | Zero_based of int
  [@@deriving sexp_of]

  let to_form = function
    | First        -> Q.first |> Form.symbol
    | Last         -> Q.last  |> Form.symbol
    | Zero_based i -> i       |> Form.int
  ;;
end

module Name = struct
  type t = Symbol.t [@@deriving sexp_of]

  let of_symbol t = t
end

let disable ?(class_ = Class.Around) name function_ =
  Symbol.funcall3_i Q.ad_disable_advice
    (function_ |> Symbol.to_value)
    (class_    |> Class.to_value)
    (name      |> Symbol.to_value)
;;

let enable ?(class_ = Class.Around) name function_ =
  Symbol.funcall3_i Q.ad_enable_advice
    (function_ |> Symbol.to_value)
    (class_    |> Class.to_value)
    (name      |> Symbol.to_value)
;;

let activate_function symbol =
  Symbol.funcall1_i Q.ad_activate (symbol |> Symbol.to_value);
;;

let deactivate_function symbol =
  Symbol.funcall1_i Q.ad_deactivate (symbol |> Symbol.to_value);
;;

let unadvise_function symbol =
  Symbol.funcall1_i Q.ad_unadvise (symbol |> Symbol.to_value);
;;

let activate_functions_with_advice_matching regexp =
  Symbol.funcall1_i Q.ad_activate_regexp (regexp |> Regexp.to_value)
;;

let deactivate_functions_with_advice_matching regexp =
  Symbol.funcall1_i Q.ad_deactivate_regexp (regexp |> Regexp.to_value)
;;

let activate_all   () = Symbol.funcall0_i Q.ad_activate_all
let deactivate_all () = Symbol.funcall0_i Q.ad_deactivate_all
let unadvise_all   () = Symbol.funcall0_i Q.ad_unadvise_all

let ad_return_value = Var.create Q.ad_return_value Value.Type.value

let defadvice
      ?(docstring = "")
      ?(position = Position.First)
      here
      ~advice_name
      ~for_function
      body
  : unit =
  let module F = Form in
  let args = Symbol.create ~name:"args" in
  let inner = Symbol.create ~name:"inner" in
  let rest_arg = Symbol.create ~name:"rest" in
  let q = F.symbol in
  F.eval_i (
    F.list
      [ q Q.defadvice
      ; q for_function
      ; F.list [ q Q.around
               ; q advice_name
               ; position |> Position.to_form ]
      ; docstring |> F.string
      ; F.let_
          [ args,  F.list [ q Q.ad_get_args; 0 |> F.int ]
          ; inner, F.lambda [%here] ~args:[] ~rest_arg
                     ~body:(F.progn
                              [ F.list [ q Q.ad_set_args
                                       ; 0 |> F.int
                                       ; q rest_arg ]
                              ; q Q.ad_do_it ])]
          (F.list (
             [ q Q.funcall
             ; F.quote (
                 Function.create here ~args:[]
                   (function _ ->
                      let args =
                        Current_buffer.value_exn (Var.create args Value.Type.(list value))
                      in
                      let inner =
                        Current_buffer.value_exn (Var.create inner Value.Type.value) in
                      Current_buffer.set_value ad_return_value
                        (body
                           ~args
                           ~inner:(fun args -> Value.funcallN inner args));
                      Value.nil)
                 |> Function.to_value)]))])
;;
