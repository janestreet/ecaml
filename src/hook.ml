open! Core_kernel
open! Import

module Q = struct
  include Q

  let add_hook = "add-hook" |> Symbol.intern
  and after_load_functions = "after-load-functions" |> Symbol.intern
  and after_revert = "after-revert-hook" |> Symbol.intern
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

module Hook_type = struct
  type 'a t =
    | File : file t
    | Normal : normal t
    | Window : window t
  [@@deriving sexp_of]
end

type 'a t =
  { var : Function.t list Var.t
  ; hook_type : 'a Hook_type.t
  }
[@@deriving fields]

let symbol t = t.var.symbol
let value_exn t = Current_buffer.value_exn t.var

let sexp_of_t _ t =
  [%message
    ""
      ~symbol:(symbol t : Symbol.t)
      ~hook_type:(t.hook_type : _ Hook_type.t)
      ~value:(value_exn t : Function.t list)]
;;

let create symbol ~hook_type =
  { var = { symbol; type_ = Value.Type.(list Function.type_) }; hook_type }
;;

module Function = struct
  type 'a t =
    { symbol : Symbol.t
    ; hook_type : 'a Hook_type.t
    }
  [@@deriving sexp_of]

  module Returns = struct
    type _ t =
      | Returns : unit Value.Type.t -> unit t
      | Returns_unit_deferred : unit Async.Deferred.t t
    [@@deriving sexp_of]

    let to_defun_returns : type a. a t -> a Defun.Returns.t = function
      | Returns t -> Returns t
      | Returns_unit_deferred -> Returns_unit_deferred
    ;;
  end

  let defun
        (type a b)
        symbol
        here
        ?docstring
        ~(hook_type : a Hook_type.t)
        (returns : b Returns.t)
        (f : a -> b)
    =
    let handle_result = function
      | Ok () -> ()
      | Error err ->
        Echo_area.message_s
          [%message "Error in hook" ~_:(symbol : Symbol.t) ~_:(err : Error.t)]
    in
    let try_with (f : unit -> b) : b =
      match returns with
      | Returns (_ : unit Value.Type.t) -> Or_error.try_with f |> handle_result
      | Returns_unit_deferred ->
        let open Async in
        Deferred.Or_error.try_with f ~extract_exn:true >>| handle_result
    in
    Defun.defun
      symbol
      here
      ?docstring
      (returns |> Returns.to_defun_returns)
      (match hook_type with
       | Normal ->
         let open Defun.Let_syntax in
         let%map_open () = return () in
         try_with f
       | Window ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and window = required Q.window Window.type_
         and start = required Q.start Position.type_ in
         try_with (fun () -> f { window; start })
       | File ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and file = required Q.file Value.Type.string in
         try_with (fun () -> f { file }))
  ;;

  let create symbol here ?docstring ~hook_type returns f =
    defun symbol here ?docstring ~hook_type returns f;
    { symbol; hook_type }
  ;;

  let create_with_self symbol here ?docstring ~hook_type returns f =
    let self = { symbol; hook_type } in
    defun symbol here ?docstring ~hook_type returns (f self);
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
let after_load = create Q.after_load_functions ~hook_type:File
let after_revert = create Q.after_revert ~hook_type:Normal
let after_save = create Q.after_save_hook ~hook_type:Normal
let before_save = create Q.before_save_hook ~hook_type:Normal
let kill_buffer = create Q.kill_buffer_hook ~hook_type:Normal

let after_load_once =
  let counter = ref 0 in
  fun f ->
    incr counter;
    let hook_function_ref = ref None in
    let hook_function =
      Function.create
        (Symbol.intern (concat [ "ecaml-after-load-"; !counter |> Int.to_string ]))
        [%here]
        ~hook_type:File
        (Returns Value.Type.unit)
        (fun file ->
           remove after_load (Option.value_exn !hook_function_ref);
           f file)
    in
    hook_function_ref := Some hook_function;
    add after_load hook_function
;;

let window_configuration_change =
  create Q.window_configuration_change_hook ~hook_type:Normal
;;

let window_scroll_functions = create Q.window_scroll_functions ~hook_type:Window
let post_command = create Q.post_command_hook ~hook_type:Normal

let major_mode_hook major_mode =
  let mode_name = major_mode |> Major_mode.symbol |> Symbol.name in
  create (mode_name ^ "-hook" |> Symbol.intern) ~hook_type:Normal
;;
