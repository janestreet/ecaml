open! Core_kernel
open! Import

module Q = struct
  include Q

  let after_load_functions = "after-load-functions" |> Symbol.intern
  let after_revert = "after-revert-hook" |> Symbol.intern
  let after_save_hook = "after-save-hook" |> Symbol.intern
  let before_save_hook = "before-save-hook" |> Symbol.intern
  let emacs_startup_hook = "emacs-startup-hook" |> Symbol.intern
  let focus_in_hook = "focus-in-hook" |> Symbol.intern
  let kill_buffer_hook = "kill-buffer-hook" |> Symbol.intern
  let post_command_hook = "post-command-hook" |> Symbol.intern

  let window_configuration_change_hook =
    "window-configuration-change-hook" |> Symbol.intern
  ;;

  let window_scroll_functions = "window-scroll-functions" |> Symbol.intern
end

include Hook0

module Function = struct
  type 'a t =
    { symbol : Symbol.t
    ; hook_type : 'a Hook_type.t
    }
  [@@deriving sexp_of]

  let defun
        (type a b)
        symbol
        here
        ?docstring
        ?should_profile
        ~(hook_type : a Hook_type.t)
        (returns : (unit, b) Defun.Returns.t)
        (f : a -> b)
    =
    let handle_result = function
      | Ok () -> ()
      | Error err ->
        message_s [%message "Error in hook" ~_:(symbol : Symbol.t) ~_:(err : Error.t)]
    in
    let try_with (f : unit -> b) : b =
      match returns with
      | Returns (_ : unit Value.Type.t) -> Or_error.try_with f |> handle_result
      | Returns_deferred (_ : unit Value.Type.t) ->
        let open Async in
        Deferred.Or_error.try_with f ~extract_exn:true >>| handle_result
    in
    Defun.defun
      symbol
      here
      ?docstring
      ?should_profile
      returns
      (match hook_type with
       | Normal ->
         let open Defun.Let_syntax in
         let%map_open () = return () in
         try_with f
       | Window ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and window = required "window" Window.t
         and start = required "start" Position.t in
         try_with (fun () -> f { window; start })
       | File ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and file = required "file" string in
         try_with (fun () -> f { file }))
  ;;

  let create symbol here ?docstring ?should_profile ~hook_type returns f =
    defun symbol here ?docstring ?should_profile ~hook_type returns f;
    { symbol; hook_type }
  ;;

  let create_with_self symbol here ?docstring ~hook_type returns f =
    let self = { symbol; hook_type } in
    defun symbol here ?docstring ~hook_type returns (f self);
    self
  ;;

  let funcall (type a) ({ symbol; hook_type } : a t) (x : a) =
    let elisp_name = symbol |> Symbol.name in
    let open Funcall.Wrap in
    match hook_type, x with
    | Normal, () -> (elisp_name <: nullary @-> return nil) ()
    | Window, { window; start } ->
      (elisp_name <: Window.t @-> Position.t @-> return nil) window start
    | File, { file } -> (elisp_name <: Value.Type.string @-> return nil) file
  ;;

  let symbol t = t.symbol
end

module Where = struct
  type t =
    | End
    | Start
  [@@deriving sexp_of]
end

let remove_hook =
  Funcall.Wrap.("remove-hook" <: Symbol.t @-> Symbol.t @-> bool @-> return nil)
;;

let remove ?(buffer_local = false) t function_ =
  remove_hook (t |> symbol) (Function.symbol function_) buffer_local
;;

module Id = Unique_id.Int ()

let make_one_shot_function_symbol function_ =
  Symbol.intern
    (concat
       [ function_ |> Function.symbol |> Symbol.name
       ; "-one-shot-"
       ; Id.create () |> Id.to_string
       ])
;;

let add_hook =
  Funcall.Wrap.("add-hook" <: Symbol.t @-> Symbol.t @-> bool @-> bool @-> return nil)
;;

let add ?(buffer_local = false) ?(one_shot = false) ?(where = Where.Start) t function_ =
  let add function_ =
    add_hook
      (t |> symbol)
      (Function.symbol function_)
      (match where with
       | End -> true
       | Start -> false)
      buffer_local
  in
  match one_shot with
  | false -> add function_
  | true ->
    let hook_function_ref = ref None in
    let hook_function =
      Function.create
        (make_one_shot_function_symbol function_)
        [%here]
        ~hook_type:function_.hook_type
        (Returns Value.Type.unit)
        (fun x ->
           remove t (Option.value_exn !hook_function_ref);
           Function.funcall function_ x)
    in
    hook_function_ref := Some hook_function;
    add hook_function
;;

let clear t = Current_buffer.set_value t.var []

let run =
  let run_hooks = Funcall.Wrap.("run-hooks" <: Symbol.t @-> return nil) in
  fun t ->
    let symbol = t |> symbol in
    Value.Private.run_outside_async [%here] (fun () -> run_hooks symbol)
;;

let after_load = create Q.after_load_functions ~hook_type:File
let after_revert = create Q.after_revert ~hook_type:Normal
let after_save = create Q.after_save_hook ~hook_type:Normal
let before_save = create Q.before_save_hook ~hook_type:Normal
let emacs_startup = create Q.emacs_startup_hook ~hook_type:Normal
let kill_buffer = create Q.kill_buffer_hook ~hook_type:Normal
let focus_in = create Q.focus_in_hook ~hook_type:Normal

let window_configuration_change =
  create Q.window_configuration_change_hook ~hook_type:Normal
;;

let window_scroll_functions = create Q.window_scroll_functions ~hook_type:Window
let post_command = create Q.post_command_hook ~hook_type:Normal

let major_mode_hook major_mode =
  let mode_name = major_mode |> Major_mode.symbol |> Symbol.name in
  create (mode_name ^ "-hook" |> Symbol.intern) ~hook_type:Normal
;;
