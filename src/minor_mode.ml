open! Core
open! Import

module Q = struct
  include Q

  let abbrev_mode = "abbrev-mode" |> Symbol.intern
  let auto_fill_mode = "auto-fill-mode" |> Symbol.intern
  let buffer_read_only = "buffer-read-only" |> Symbol.intern
  let define_minor_mode = "define-minor-mode" |> Symbol.intern
  let define_keymap = "define-keymap" |> Symbol.intern
  let goto_address_mode = "goto-address-mode" |> Symbol.intern
  let hl_line_mode = "hl-line-mode" |> Symbol.intern
  let read_only_mode = "read-only-mode" |> Symbol.intern
  let url_handler_mode = "url-handler-mode" |> Symbol.intern
  let visual_line_mode = "visual-line-mode" |> Symbol.intern
end

type t =
  { function_name : Symbol.t
  ; variable_name : Symbol.t
  }
[@@deriving fields ~getters, sexp_of]

let create ?variable_name function_name =
  { function_name; variable_name = Option.value variable_name ~default:function_name }
;;

let wrap_existing name =
  let name = Symbol.intern name in
  { function_name = name; variable_name = name }
;;

let compare_name a b = Comparable.lift Symbol.compare_name ~f:function_name a b
let abbrev = { function_name = Q.abbrev_mode; variable_name = Q.abbrev_mode }
let auto_fill = { function_name = Q.auto_fill_mode; variable_name = Q.auto_fill_mode }

let goto_address =
  { function_name = Q.goto_address_mode; variable_name = Q.goto_address_mode }
;;

let hl_line = { function_name = Q.hl_line_mode; variable_name = Q.hl_line_mode }
let read_only = { function_name = Q.read_only_mode; variable_name = Q.buffer_read_only }
let button = wrap_existing "button-mode"

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
    Sync_or_async.protect sync_or_async ~f ~finally:(fun () -> enable t))
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
  ?initialize
  ()
  =
  let docstring = docstring |> String.strip in
  require_nonempty_docstring here ~docstring;
  let keymap_var = Var.Wrap.(concat [ name |> Symbol.name; "-map" ] <: Keymap.t) in
  let docstring =
    concat
      [ docstring
      ; "\n\n"
      ; Documentation.Special_sequence.describe_keymap keymap_var.symbol
      ]
  in
  let t = { function_name = name; variable_name = name } in
  let initialize =
    Option.map initialize ~f:(fun f ->
      Defun.defun_func
        ([%string "%{Symbol.name name}--init"] |> Symbol.intern)
        here
        ~docstring:Documentation.O.([%string "Initializer for %{symbol name}."])
        (Returns Value.Type.unit)
        (let%map_open.Defun () = return () in
         f t)
      |> Function.to_value
      |> Symbol.of_value_exn)
  in
  Dump.eval_and_dump ~here (fun () ->
    Form.list
      ([ Q.define_minor_mode |> Form.symbol
       ; name |> Form.symbol
       ; docstring |> String.strip |> Form.string
       ; Q.K.lighter |> Form.symbol
       ; Option.value_map
           mode_line
           ~f:(fun mode_line -> String.concat [ " "; mode_line ] |> Form.string)
           ~default:Form.nil
       ; Q.K.keymap |> Form.symbol
       ; Form.apply
           Q.define_keymap
           (List.concat_map define_keys ~f:(fun (key, symbol) ->
              [ Form.string key; Form.quote (Symbol.to_value symbol) ]))
       ]
       @ (match global with
          | None -> []
          | Some group ->
            [ Q.K.global |> Form.symbol
            ; Value.of_bool true |> Form.of_value_exn
            ; Q.K.group |> Form.symbol
            ; group |> Customization.Group.to_value |> Form.quote
            ])
       @
       match initialize with
       | None -> []
       | Some initialize -> [ Form.apply initialize [] ]));
  Load_history.add_entry here (Fun name);
  Load_history.add_entry here (Var (Var.symbol keymap_var));
  List.iter [ "hook"; "on-hook"; "off-hook" ] ~f:(fun suffix ->
    Load_history.add_entry
      here
      (Var (concat [ name |> Symbol.name; "-"; suffix ] |> Symbol.intern)));
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
  Var.Wrap.(concat [ t.function_name |> Symbol.name; "-map" ] <: Keymap.t)
;;
