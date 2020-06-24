open! Core_kernel
open! Import

module Q = struct
  include Q

  let abbrev_mode = "abbrev-mode" |> Symbol.intern
  let buffer_read_only = "buffer-read-only" |> Symbol.intern
  let define_minor_mode = "define-minor-mode" |> Symbol.intern
  let goto_address_mode = "goto-address-mode" |> Symbol.intern
  let read_only_mode = "read-only-mode" |> Symbol.intern
  let url_handler_mode = "url-handler-mode" |> Symbol.intern
  let view_mode = "view-mode" |> Symbol.intern
  let visual_line_mode = "visual-line-mode" |> Symbol.intern
end


type t =
  { function_name : Symbol.t
  ; variable_name : Symbol.t
  }
[@@deriving fields, sexp_of]

let compare_name = Comparable.lift Symbol.compare_name ~f:function_name
let abbrev = { function_name = Q.abbrev_mode; variable_name = Q.abbrev_mode }

let goto_address =
  { function_name = Q.goto_address_mode; variable_name = Q.goto_address_mode }
;;

let read_only = { function_name = Q.read_only_mode; variable_name = Q.buffer_read_only }
let view = { function_name = Q.view_mode; variable_name = Q.view_mode }

let url_handler =
  { function_name = Q.url_handler_mode; variable_name = Q.url_handler_mode }
;;

let visual_line =
  { function_name = Q.visual_line_mode; variable_name = Q.visual_line_mode }
;;

let is_enabled t =
  Current_buffer.value { symbol = t.variable_name; type_ = Value.Type.bool }
  |> Option.value ~default:false
;;

let disable t = Symbol.funcall1_i t.function_name (0 |> Value.of_int_exn)
let enable t = Symbol.funcall1_i t.function_name (1 |> Value.of_int_exn)

let temporarily_disable sync_or_async t ~f =
  if is_enabled t
  then (
    disable t;
    Sync_or_async.protect [%here] sync_or_async ~f ~finally:(fun () -> enable t))
  else f ()
;;

let all_minor_modes = ref []

module Private = struct
  let all_minor_modes () = !all_minor_modes |> List.sort ~compare:compare_name
end

let define_minor_mode
      name
      here
      ~docstring
      ?(define_keys = [])
      ?mode_line
      ~global
      ?(initialize = fun () -> ())
      ()
  =
  let keymap_var = Var.Wrap.(concat [ name |> Symbol.name; "-map" ] <: Keymap.t) in
  Current_buffer.set_value keymap_var (Keymap.create ());
  let keymap = Current_buffer.value_exn keymap_var in
  List.iter define_keys ~f:(fun (keys, symbol) ->
    Keymap.define_key keymap (Key_sequence.create_exn keys) (Symbol symbol));
  let docstring =
    concat
      [ String.strip docstring
      ; "\n\n"
      ; Documentation.Special_sequence.keymap keymap_var.symbol
      ]
  in
  Form.Blocking.eval_i
    (Form.list
       [ Q.define_minor_mode |> Form.symbol
       ; name |> Form.symbol
       ; docstring |> String.strip |> Form.string
       ; Q.K.lighter |> Form.symbol
       ; Option.value_map
           mode_line
           ~f:(fun mode_line -> String.concat [ " "; mode_line ] |> Form.string)
           ~default:Form.nil
       ; Q.K.keymap |> Form.symbol
       ; Var.symbol_as_value keymap_var |> Form.of_value_exn
       ; Q.K.global |> Form.symbol
       ; Value.of_bool global |> Form.of_value_exn
       ; Form.list
           [ Q.funcall |> Form.symbol
           ; Form.quote (Defun.lambda_nullary_nil here initialize |> Function.to_value)
           ]
       ]);
  let t = { function_name = name; variable_name = name } in
  all_minor_modes := t :: !all_minor_modes;
  t
;;

let keymap t =
  List.Assoc.find
    (Current_buffer.value_exn Keymap.minor_mode_map_alist)
    t.function_name
    ~equal:Symbol.equal
;;

let keymap_exn t =
  match keymap t with
  | Some x -> x
  | None -> raise_s [%message "minor mode has no keymap" ~minor_mode:(t : t)]
;;
