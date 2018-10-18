open! Core_kernel
open! Import

module Q = struct
  include Q

  let add_hook = "add-hook" |> Symbol.intern
  and after_load_functions = "after-load-functions" |> Symbol.intern
  and after_save_hook = "after-save-hook" |> Symbol.intern
  and before_save_hook = "before-save-hook" |> Symbol.intern
  and kill_buffer_hook = "kill-buffer-hook" |> Symbol.intern
  and post_command_hook = "post-command-hook" |> Symbol.intern
  and remove_hook = "remove-hook" |> Symbol.intern
  and run_hooks = "run-hooks" |> Symbol.intern
  and start = "start" |> Symbol.intern
  and window = "window" |> Symbol.intern
  and window_configuration_change_hook =
    "window-configuration-change-hook" |> Symbol.intern
  and window_scroll_functions = "window-scroll-functions" |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let add_hook =
    Q.add_hook <: Symbol.type_ @-> Symbol.type_ @-> bool @-> bool @-> return nil
  and remove_hook =
    Q.remove_hook <: Symbol.type_ @-> Symbol.type_ @-> bool @-> return nil
  and run_hooks = Q.run_hooks <: Symbol.type_ @-> return nil
end

type file = { file : string } [@@deriving sexp_of]
type normal = unit [@@deriving sexp_of]

type window =
  { window : Window.t
  ; start : Position.t
  }
[@@deriving sexp_of]

module Type = struct
  type 'a t =
    | File : file t
    | Normal : normal t
    | Window : window t
  [@@deriving sexp_of]

  let args : type a. a t -> _ = function
    | File -> [ Q.file ]
    | Normal -> []
    | Window -> [ Q.window; Q.start ]
  ;;

  let fn (type a) (t : a t) (f : a -> unit) ~symbol : Function.Fn.t =
    let wrap f =
      match Or_error.try_with f with
      | Ok () -> ()
      | Error err ->
        Echo_area.message_s
          [%message "Error in hook" ~_:(symbol : Symbol.t) ~_:(err : Error.t)]
    in
    match t with
    | Normal ->
      (function
        | [|  |] ->
          wrap f;
          Value.nil
        | _ -> assert false)
    | Window ->
      (function
        | [| window; start |] ->
          wrap (fun () ->
            f
              { window = window |> Window.of_value_exn
              ; start = start |> Position.of_value_exn
              });
          Value.nil
        | _ -> assert false)
    | File ->
      (function
        | [| file |] ->
          wrap (fun () -> f { file = file |> Value.to_utf8_bytes_exn });
          Value.nil
        | _ -> assert false)
  ;;
end

type 'a t =
  { var : Function.t list Var.t
  ; type_ : 'a Type.t
  }
[@@deriving fields]

let symbol t = t.var.symbol
let value_exn t = Current_buffer.value_exn t.var

let sexp_of_t _ t =
  [%message
    ""
      ~symbol:(symbol t : Symbol.t)
      ~type_:(t.type_ : _ Type.t)
      ~value:(value_exn t : Function.t list)]
;;

let create type_ symbol =
  { var = { symbol; type_ = Value.Type.(list Function.type_) }; type_ }
;;

module Function = struct
  type 'a t =
    { symbol : Symbol.t
    ; type_ : 'a Type.t
    }
  [@@deriving sexp_of]

  module Return_type = struct
    type timeout = { timeout : Time.Span.t option } [@@deriving sexp_of]

    type _ t =
      | Unit : unit t
      | Unit_deferred : timeout -> unit Async.Deferred.t t
    [@@deriving sexp_of]

    let to_unit (type a r) (f : a -> r) (return_type : r t) : a -> unit =
      match return_type with
      | Unit -> f
      | Unit_deferred { timeout } ->
        fun a -> Async_ecaml.Private.block_on_async ~timeout (fun () -> f a)
    ;;
  end

  let create ?docstring here type_ return_type symbol f =
    Defun.defun_raw
      ?docstring
      here
      symbol
      ~args:(Type.args type_)
      (Type.fn ~symbol type_ (Return_type.to_unit f return_type));
    { symbol; type_ }
  ;;

  let create_with_self ?docstring here type_ return_type symbol f =
    let self = { symbol; type_ } in
    Defun.defun_raw
      ?docstring
      here
      symbol
      ~args:(Type.args type_)
      (Type.fn ~symbol type_ (Return_type.to_unit (f self) return_type));
    self
  ;;

  let symbol t = t.symbol
end

module Where = struct
  type t =
    | End
    | Start
  [@@deriving sexp_of]
end

let add ?(buffer_local = false) ?(where = Where.Start) t function_ =
  F.add_hook
    (t |> symbol)
    (Function.symbol function_)
    (match where with
     | End -> true
     | Start -> false)
    buffer_local
;;

let remove ?(buffer_local = false) t function_ =
  F.remove_hook (t |> symbol) (Function.symbol function_) buffer_local
;;

let clear t = Current_buffer.set_value t.var []
let run t = F.run_hooks (t |> symbol)
let after_load = create File Q.after_load_functions
let after_save = create Normal Q.after_save_hook
let before_save = create Normal Q.before_save_hook
let kill_buffer = create Normal Q.kill_buffer_hook

let after_load_once =
  let counter = ref 0 in
  fun f ->
    incr counter;
    let hook_function_ref = ref None in
    let hook_function =
      Function.create
        [%here]
        File
        Unit
        (Symbol.intern (concat [ "ecaml-after-load-"; !counter |> Int.to_string ]))
        (fun file ->
           remove after_load (Option.value_exn !hook_function_ref);
           f file)
    in
    hook_function_ref := Some hook_function;
    add after_load hook_function
;;

let window_configuration_change = create Normal Q.window_configuration_change_hook
let window_scroll_functions = create Window Q.window_scroll_functions
let post_command = create Normal Q.post_command_hook

let major_mode_hook major_mode =
  let mode_name = major_mode |> Major_mode.symbol |> Symbol.name in
  create Normal (mode_name ^ "-hook" |> Symbol.intern)
;;
