open! Core
open! Async_kernel
open! Import

type t =
  { func : Function.t
  ; background_safe : bool
  }

let define ~(here : [%call_pos]) symbol f =
  { func =
      Defun.defun_func
        symbol
        here
        ~docstring:"A `revert-buffer-function'."
        (Returns_deferred Value.Type.unit)
        (let%map_open.Defun () = return ()
         and () = required "ignore-auto" ignored
         and noconfirm = required "noconfirm" bool in
         f ~confirm:(not noconfirm))
  ; background_safe = false
  }
;;

let lambda ~(here : [%call_pos]) f =
  { func =
      Defun.lambda
        here
        (Returns_deferred Value.Type.unit)
        (let%map_open.Defun () = return ()
         and () = required "ignore-auto" ignored
         and noconfirm = required "noconfirm" bool in
         f ~confirm:(not noconfirm))
  ; background_safe = false
  }
;;

let lambda_background_safe ~(here : [%call_pos]) f =
  { func =
      Defun.lambda
        here
        (Returns_deferred Value.Type.unit)
        (let%map_open.Defun () = return ()
         and () = required "ignore-auto" ignored
         and noconfirm = required "noconfirm" bool
         and buffer = optional "buffer" Buffer.t in
         f
           ~confirm:(not noconfirm)
           (Option.value_or_thunk ~default:Current_buffer.get buffer))
  ; background_safe = true
  }
;;

let revert_buffer_function =
  Buffer_local.Wrap.(
    let ( <: ) = ( <: ) ~make_buffer_local_always:true in
    "revert-buffer-function" <: Function.t)
;;

let ecaml_revert_is_background_safe =
  Buffer_local.defvar
    ("ecaml-revert-is-background-safe" |> Symbol.intern)
    ~docstring:
      {|Non-nil if `revert-buffer-function' can be called in the background by Async.

If so, `revert-buffer-function' will have a third optional argument,
the buffer to use, which should be passed when calling it in the background.|}
    ~type_:Value.Type.bool
    ~default_value:false
    ()
;;

let set buffer { func; background_safe } =
  Buffer_local.set revert_buffer_function func buffer;
  Buffer_local.set ecaml_revert_is_background_safe background_safe buffer
;;

let get buffer =
  { func = Buffer_local.get revert_buffer_function buffer
  ; background_safe = Buffer_local.get ecaml_revert_is_background_safe buffer
  }
;;

let revert_background_safe_exn buffer =
  if Buffer_local.get ecaml_revert_is_background_safe buffer
  then (
    let func = Buffer_local.get revert_buffer_function buffer |> Function.to_value in
    let buffer = Buffer.to_value buffer in
    Value.Private.run_outside_async ~allowed_in_background:true (fun () ->
      Value.funcall3_i func Value.nil Value.t buffer))
  else raise_s [%sexp "Buffer can't be reverted in the background", (buffer : Buffer.t)]
;;
