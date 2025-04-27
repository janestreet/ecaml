open! Core
open! Import

type t = Symbol.t [@@deriving sexp_of]

let add =
  let advice_add =
    Funcall.Wrap.("advice-add" <: Symbol.t @-> Symbol.t @-> Symbol.t @-> return nil)
  in
  fun t ~to_function -> advice_add to_function Q.K.around t
;;

let of_function symbol = symbol

let defun_internal
  advice_name
  ~here
  ~docstring
  ?interactive
  ?should_profile
  sync_or_async
  f
  =
  Defun.defun
    advice_name
    here
    ~docstring
    ?interactive
    ?should_profile
    (Defun.Returns.returns sync_or_async Value.Type.value)
    (let open Defun.Let_syntax in
     let%map_open () = return ()
     and inner = required "inner" value
     and rest = rest "rest" value in
     f inner rest);
  advice_name
;;

let defun_around_values
  advice_name
  ~(here : [%call_pos])
  sync_or_async
  ~docstring
  ?interactive
  ?should_profile
  f
  =
  defun_internal
    advice_name
    ~here
    ~docstring
    ?interactive
    ?should_profile
    sync_or_async
    (fun inner rest -> f (Value.funcallN ?should_profile inner) rest)
;;

module On_parse_error = struct
  type t =
    | Allow_raise
    | Call_inner_function
  [@@deriving sexp_of]
end

let defun_around_funcall
  advice_name
  ~(here : [%call_pos])
  ~docstring
  ?interactive
  ?(on_parse_error = On_parse_error.Allow_raise)
  ?should_profile
  funcall
  f
  =
  defun_internal
    advice_name
    ~here
    ~docstring
    ?interactive
    ?should_profile
    Sync
    (fun inner rest ->
       Funcall.Private.apply
         funcall
         (f (Funcall.Private.wrap_unrolled funcall inner))
         rest
         ~on_parse_error:
           (match (on_parse_error : On_parse_error.t) with
            | Allow_raise ->
              fun exn ->
                raise_s
                  [%message
                    "Advice failed to parse its arguments"
                      ~_:(here : Source_code_position.t)
                      ~_:(exn : exn)]
            | Call_inner_function ->
              fun exn ->
                Echo_area.inhibit_messages Sync (fun () ->
                  message_s
                    [%message
                      "Ignoring advice that failed to parse its arguments"
                        ~_:(here : Source_code_position.t)
                        ~_:(exn : exn)]);
                Value.funcallN inner rest))
;;

let remove =
  let advice_remove =
    Funcall.Wrap.("advice-remove" <: Symbol.t @-> Symbol.t @-> return nil)
  in
  fun t ~from_function -> advice_remove from_function t
;;
