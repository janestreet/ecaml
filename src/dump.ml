open! Core
open! Import

module Q = struct
  let declare_function = "declare-function" |> Symbol.intern
  let ecaml_dumped = "ecaml-dumped" |> Symbol.intern
  let pp_emacs_lisp_code = "pp-emacs-lisp-code" |> Symbol.intern
  let keymap_set = "keymap-set" |> Symbol.intern
  let progn = "progn" |> Symbol.intern
end

let pp_default_function = Var.Wrap.("pp-default-function" <: Symbol.t)

let pp =
  let pp = Funcall.Wrap.("pp" <: Form.t @-> return ignored) in
  fun form ->
    Current_buffer0.set_value_temporarily
      Sync
      pp_default_function
      Q.pp_emacs_lisp_code
      ~f:(fun () -> pp form)
;;

let princ = Funcall.Wrap.("princ" <: string @-> return ignored)

(* This defconst will only be defined by the dump; if it's not defined, we weren't loaded
   from a dump. *)
let ecaml_dumped = Current_buffer0.variable_is_defined Q.ecaml_dumped
let ecaml_dumping = Sys.getenv "ECAML_DUMPING" |> Option.is_some

(* This is set to false at the end of module initialization. *)
let in_module_initialization = ref true
let allow_dump_override = ref false

let with_allowed_dump_for_testing f =
  assert am_running_test;
  Ref.set_temporarily allow_dump_override true ~f
;;

let assert_can_dump here =
  if (not !in_module_initialization) && not !allow_dump_override
  then
    message_s
      [%sexp
        "Tried to call a Dump function after module initialization"
        , (here : Source_code_position.t)]
;;

let eval_and_dump ~(here : Source_code_position.t) make_form =
  assert_can_dump here;
  let should_eval = not ecaml_dumped in
  let should_dump = ecaml_dumping in
  if should_eval || should_dump
  then (
    (* If we're not evaling it or dumping it, don't run the code to make the form, that
       would just waste time. *)
    let form = make_form () in
    if should_dump
    then (
      princ [%string "; defined by %{here.pos_fname}\n"];
      pp form);
    if should_eval then Form.Blocking.eval_i form)
;;

let defalias =
  Funcall.Wrap.("defalias" <: Symbol.t @-> value @-> nil_or string @-> return nil)
;;

let ecaml_plugin_so = Form.string "ecaml_plugin.so"
let t = Form.of_value_exn Value.t

let defalias ~here symbol value =
  assert_can_dump here;
  let should_dump = ecaml_dumping in
  if should_dump
  then pp (Form.apply Q.declare_function [ Form.symbol symbol; ecaml_plugin_so; t; t ]);
  Load_history.add_entry here (Fun symbol);
  defalias symbol value None
;;

let keymap_set_form keymap (key, def) =
  Form.apply
    Q.keymap_set
    [ Var.symbol keymap |> Form.symbol
    ; Form.string key
    ; Form.quote (Symbol.to_value def)
    ]
;;

let keymap_set ~here keymap keydefs =
  match keydefs with
  | [] -> ()
  | [ keydef ] -> eval_and_dump ~here (fun () -> keymap_set_form keymap keydef)
  | keydefs ->
    eval_and_dump ~here (fun () ->
      Form.apply Q.progn (List.map keydefs ~f:(keymap_set_form keymap)))
;;

let () =
  Ecaml_value.Ecaml_callback.on_end_of_module_initialization
    (fun `Not_holding_the_async_lock -> in_module_initialization := false)
;;
