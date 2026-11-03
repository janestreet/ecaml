open! Core
open! Async_kernel
open! Import

type t =
  { func : Function.t
  ; takes_explicit_buffer : bool
  }

let define ?(here = Stdlib.Lexing.dummy_pos) symbol f =
  { func =
      Defun.defun_func
        symbol
        here
        ~docstring:
          {|
A `revert-buffer-function'.

See the documentation of that variable for the meaning of
IGNORE-AUTO and NOCONFIRM.
|}
        (Returns_deferred Value.Type.unit)
        (let%map_open.Defun () = return ()
         and () = required "ignore-auto" ignored
         and noconfirm = required "noconfirm" bool in
         f ~confirm:(not noconfirm))
  ; takes_explicit_buffer = false
  }
;;

let lambda ?(here = Stdlib.Lexing.dummy_pos) f =
  { func =
      Defun.lambda
        here
        (Returns_deferred Value.Type.unit)
        (let%map_open.Defun () = return ()
         and () = required "ignore-auto" ignored
         and noconfirm = required "noconfirm" bool in
         f ~confirm:(not noconfirm))
  ; takes_explicit_buffer = false
  }
;;

let define_explicit_buffer ?(here = Stdlib.Lexing.dummy_pos) symbol sync_or_async f =
  { func =
      Defun.defun_func
        symbol
        here
        ~docstring:
          {|
A `revert-buffer-function' that takes an optional BUFFER argument.

If BUFFER is non-nil, revert it instead of the current buffer.

See the documentation of `revert-buffer-function' for the meaning of
IGNORE-AUTO and NOCONFIRM.
|}
        (Defun.Returns.returns sync_or_async Value.Type.unit)
        (let%map_open.Defun () = return ()
         and () = required "ignore-auto" ignored
         and noconfirm = required "noconfirm" bool
         and buffer = optional "buffer" Buffer.t in
         f
           ~confirm:(not noconfirm)
           (Option.value_or_thunk ~default:Current_buffer.get buffer))
  ; takes_explicit_buffer = true
  }
;;

let revert_buffer_function =
  Buffer_local.Wrap.(
    let ( <: ) = ( <: ) ~make_buffer_local_always:true in
    "revert-buffer-function" <: Function.t)
;;

let ecaml_revert_takes_explicit_buffer =
  Buffer_local.defvar
    ("ecaml-revert-takes-explicit-buffer" |> Symbol.intern)
    ~docstring:
      {|Non-nil if `revert-buffer-function' takes the buffer to revert as an argument.

If so, it has a third optional argument, the buffer to revert, so it can
be called to revert a buffer other than the current buffer.|}
    ~type_:Value.Type.bool
    ~default_value:false
    ()
;;

let set buffer { func; takes_explicit_buffer } =
  Buffer_local.set revert_buffer_function func buffer;
  Buffer_local.set ecaml_revert_takes_explicit_buffer takes_explicit_buffer buffer
;;

module Blocking = struct
  let revert_explicit_buffer_exn buffer =
    if Buffer_local.get ecaml_revert_takes_explicit_buffer buffer
    then (
      let func = Buffer_local.get revert_buffer_function buffer |> Function.to_value in
      Value.funcall3_i func Value.nil Value.t (Buffer.to_value buffer))
    else
      raise_s
        [%sexp
          "Buffer's revert-buffer-function doesn't take an explicit buffer"
          , (buffer : Buffer.t)]
  ;;
end

let revert_explicit_buffer_exn buffer =
  Value.Private.run_outside_async (fun () -> Blocking.revert_explicit_buffer_exn buffer)
;;
