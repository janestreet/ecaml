open! Core
open! Import
include Hook0

module Function = struct
  type ('a, 'b) t =
    { symbol : Symbol.t
    ; hook_type : ('a, 'b) Hook_type.t
    }
  [@@deriving sexp_of]

  let defun
    (type a b r)
    symbol
    here
    ~docstring
    ?should_profile
    ~(hook_type : (a, b) Hook_type.t)
    (returns : (b, r) Defun.Returns.t)
    (f : a -> r)
    =
    let handle_unit_result = function
      | Ok () -> ()
      | Error err ->
        message_s [%message "Error in hook" ~_:(symbol : Symbol.t) ~_:(err : Error.t)]
    in
    let handle_bool_result = function
      | Ok b -> b
      | Error err ->
        (* We failed to query the user, so the safest thing to do is to abort whatever
           operation wanted to query in the first place. *)
        raise_s [%message "Error in hook" ~_:(symbol : Symbol.t) ~_:(err : Error.t)]
    in
    let handle_result : b Or_error.t -> b =
      match hook_type with
      | After_change_hook -> handle_unit_result
      | Before_change_hook -> handle_unit_result
      | File_hook -> handle_unit_result
      | Normal_hook -> handle_unit_result
      | Frame_hook -> handle_unit_result
      | Window_hook -> handle_unit_result
      | Query_function -> handle_bool_result
    in
    let try_with (f : unit -> r) : r =
      match returns with
      | Returns (_ : b Value.Type.t) -> Or_error.try_with f |> handle_result
      | Returns_deferred (_ : b Value.Type.t) ->
        let open Async in
        Deferred.Or_error.try_with f ~extract_exn:true >>| handle_result
    in
    Defun.defun
      symbol
      here
      ~docstring
      ?should_profile
      returns
      (match hook_type with
       | Normal_hook ->
         let open Defun.Let_syntax in
         let%map_open () = return () in
         try_with f
       | Frame_hook ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and frame = required "frame" Frame.t in
         try_with (fun () -> f { frame })
       | Window_hook ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and window = required "window" Window.t
         and start = required "start" Position.t in
         try_with (fun () -> f { window; start })
       | File_hook ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and file = required "file" string in
         try_with (fun () -> f { file })
       | Before_change_hook ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and beginning_of_changed_region =
           required "beginning-of-changed-region" Position.t
         and end_of_changed_region = required "end-of-changed-region" Position.t in
         try_with (fun () -> f { beginning_of_changed_region; end_of_changed_region })
       | After_change_hook ->
         let open Defun.Let_syntax in
         let%map_open () = return ()
         and beginning_of_changed_region =
           required "beginning-of-changed-region" Position.t
         and end_of_changed_region = required "end-of-changed-region" Position.t
         and length_before_change = required "length-before-change" int in
         try_with (fun () ->
           f { beginning_of_changed_region; end_of_changed_region; length_before_change })
       | Query_function ->
         let open Defun.Let_syntax in
         let%map_open () = return () in
         try_with f)
  ;;

  let create symbol here ~docstring ?should_profile ~hook_type returns f =
    defun symbol here ~docstring ?should_profile ~hook_type returns f;
    { symbol; hook_type }
  ;;

  let create_with_self symbol here ~docstring ~hook_type returns f =
    let self = { symbol; hook_type } in
    defun symbol here ~docstring ~hook_type returns (f self);
    self
  ;;

  let wrap symbol ~hook_type = { symbol; hook_type }

  let funcall (type a b) ({ symbol; hook_type } : (a, b) t) (x : a) : b =
    let elisp_name = symbol |> Symbol.name in
    let open Funcall.Wrap in
    match hook_type, x with
    | Normal_hook, () -> (elisp_name <: nullary @-> return nil) ()
    | Frame_hook, { frame } -> (elisp_name <: Frame.t @-> return nil) frame
    | Window_hook, { window; start } ->
      (elisp_name <: Window.t @-> Position.t @-> return nil) window start
    | File_hook, { file } -> (elisp_name <: string @-> return nil) file
    | Before_change_hook, { beginning_of_changed_region; end_of_changed_region } ->
      (elisp_name <: Position.t @-> Position.t @-> return nil)
        beginning_of_changed_region
        end_of_changed_region
    | ( After_change_hook
      , { beginning_of_changed_region; end_of_changed_region; length_before_change } ) ->
      (elisp_name <: Position.t @-> Position.t @-> int @-> return nil)
        beginning_of_changed_region
        end_of_changed_region
        length_before_change
    | Query_function, () -> (elisp_name <: nullary @-> return bool) ()
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

let remove_symbol ?(buffer_local = false) t symbol_ =
  remove_hook (t |> symbol) symbol_ buffer_local
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

let add
  (type a b)
  ?(buffer_local = false)
  ?(one_shot = false)
  ?(where = Where.Start)
  (t : (a, b) t)
  function_
  =
  let add function_ =
    add_hook
      (t |> symbol)
      (Function.symbol function_)
      (match where with
       | End -> true
       | Start -> false)
      buffer_local
  in
  let type_ : b Value.Type.t =
    match t.hook_type with
    | After_change_hook -> Value.Type.unit
    | Before_change_hook -> Value.Type.unit
    | File_hook -> Value.Type.unit
    | Normal_hook -> Value.Type.unit
    | Frame_hook -> Value.Type.unit
    | Window_hook -> Value.Type.unit
    | Query_function -> Value.Type.bool
  in
  match one_shot with
  | false -> add function_
  | true ->
    let hook_function_ref = ref None in
    let hook_function =
      Function.create
        (make_one_shot_function_symbol function_)
        [%here]
        ~docstring:"One-shot hook function."
        ~hook_type:function_.hook_type
        (Returns type_)
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

let after_change_functions = Wrap.("after-change-functions" <: After_change_hook)
let after_load = Wrap.("after-load-functions" <: File_hook)
let after_revert = Wrap.("after-revert-hook" <: Normal_hook)
let after_save = Wrap.("after-save-hook" <: Normal_hook)
let before_change_functions = Wrap.("before-change-functions" <: Before_change_hook)
let before_save = Wrap.("before-save-hook" <: Normal_hook)
let change_major_mode = Wrap.("change-major-mode-hook" <: Normal_hook)
let emacs_startup = Wrap.("emacs-startup-hook" <: Normal_hook)
let kill_buffer = Wrap.("kill-buffer-hook" <: Normal_hook)
let focus_in = Wrap.("focus-in-hook" <: Normal_hook)
let window_configuration_change = Wrap.("window-configuration-change-hook" <: Normal_hook)
let window_scroll_functions = Wrap.("window-scroll-functions" <: Window_hook)
let post_command = Wrap.("post-command-hook" <: Normal_hook)
let pre_command = Wrap.("pre-command-hook" <: Normal_hook)
let server_after_make_frame = Wrap.("server-after-make-frame-hook" <: Normal_hook)
let project_find_functions = Wrap.("project-find-functions" <: File_hook)

let major_mode_hook major_mode =
  let mode_name = major_mode |> Major_mode.symbol |> Symbol.name in
  Wrap.(mode_name ^ "-hook" <: Normal_hook)
;;
