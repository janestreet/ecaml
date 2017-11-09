open! Core_kernel
open! Import

type file   = file : string -> unit [@@deriving sexp_of]
type normal = unit          -> unit [@@deriving sexp_of]

module Type = struct
  type 'a t =
    | File   : file   t
    | Normal : normal t
  [@@deriving sexp_of]

  let args : type a . a t -> _ = function
    | File -> [ Q.file ]
    | Normal -> []
  ;;

  let fn (type a) (t : a t) (f : a) ~symbol : Function.Fn.t =
    let wrap f =
      match Or_error.try_with f with
      | Ok () -> ()
      | Error err ->
        Echo_area.message_s
          [%message "Error in hook"
                      ~_:(symbol : Symbol.t)
                      ~_:(err : Error.t)]
    in
    match t with
    | Normal ->
      (function
        | [| |] -> wrap f; Value.nil
        | _ -> assert false)
    | File   ->
      (function
        | [| file |] ->
          wrap (fun () -> f ~file:(file |> Value.to_utf8_bytes_exn)); Value.nil
        | _ -> assert false)
  ;;
end

type 'a t =
  { var    : Function.t list Var.t
  ; type_  : 'a Type.t }
[@@deriving fields]

let symbol t = t.var.symbol

let symbol_as_value t = symbol t |> Symbol.to_value

let value_exn t = Current_buffer.value_exn t.var

let sexp_of_t _ t =
  [%message
    ""
      ~symbol:(symbol t : Symbol.t)
      ~type_:(t.type_ : _ Type.t)
      ~value:(value_exn t : Function.t list)]
;;

let create type_ symbol =
  { var = { symbol; type_ = Value.Type.(list Function.type_) }
  ; type_ }
;;

module Function = struct
  type 'a t =
    { symbol : Symbol.t
    ; type_  : 'a Type.t }
  [@@deriving sexp_of]

  let create ?docstring here type_ symbol f =
    Function.defun ?docstring here symbol ~args:(Type.args type_)
      (Type.fn ~symbol type_ f);
    { symbol; type_ }
  ;;

  let to_value t = t.symbol |> Symbol.to_value
end

module Where = struct
  type t =
    | End
    | Start
  [@@deriving sexp_of]
end

let add ?(buffer_local = false) ?(where = Where.Start) t function_ =
  Symbol.funcall4_i Q.add_hook
    (t |> symbol_as_value)
    (function_ |> Function.to_value)
    (match where with End -> Value.t | Start -> Value.nil)
    (buffer_local |> Value.of_bool);
;;

let remove t function_ =
  Symbol.funcall2_i Q.remove_hook (t |> symbol_as_value) (function_ |> Function.to_value)
;;

let clear t = Current_buffer.set_value t.var []

let run t = Symbol.funcall1_i Q.run_hooks (t |> symbol_as_value)

let after_load = create File Q.after_load_functions

let after_save  = create Normal Q.after_save_hook

let kill_buffer = create Normal Q.kill_buffer_hook

let after_load_once =
  let counter = ref 0 in
  fun f ->
    incr counter;
    let hook_function_ref = ref None in
    let hook_function =
      Function.create [%here] File
        (Symbol.intern
           (concat [ "ecaml-after-load-"; !counter |> Int.to_string ]))
        (fun ~file ->
           remove after_load (Option.value_exn !hook_function_ref);
           f ~file) in
    hook_function_ref := Some hook_function;
    add after_load hook_function;
;;
