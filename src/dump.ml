open! Core
open! Import

module Q = struct
  let pp_emacs_lisp_code = "pp-emacs-lisp-code" |> Symbol.intern
  let keymap_set = "keymap-set" |> Symbol.intern
  let progn = "progn" |> Symbol.intern
  let declare_function = "declare-function" |> Symbol.intern
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
let ecaml_dumping = Sys.getenv "ECAML_DUMPING" |> Option.is_some
let should_eval_when_dumping = am_running_test

(* This is set to false at the end of module initialization. *)
let in_module_initialization = ref true
let allow_dump_override = ref false

let assert_can_dump here =
  if (not !in_module_initialization) && not !allow_dump_override
  then
    raise_s
      [%sexp
        "Tried to call a Dump function after module initialization"
        , (here : Source_code_position.t)]
;;

let dump_or_eval ~(here : Source_code_position.t) make_form =
  if ecaml_dumping
  then (
    princ [%string "; defined by %{here.pos_fname}\n"];
    pp (make_form ()))
  else if (not !in_module_initialization)
          || should_eval_when_dumping
          || !allow_dump_override
  then Form.Blocking.eval_i (make_form ())
;;

let eval_and_dump ~(here : Source_code_position.t) make_form =
  assert_can_dump here;
  dump_or_eval ~here make_form
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

let declare_function ~(here : [%call_pos]) fn ~file =
  dump_or_eval ~here (fun () ->
    Form.apply
      Q.declare_function
      [ Form.symbol fn; Form.string file; Value.t |> Form.of_value_exn ])
;;

let () =
  Ecaml_value.Ecaml_callback.on_end_of_module_initialization
    (fun `Not_holding_the_async_lock -> in_module_initialization := false)
;;

module For_testing = struct
  let allow_calls_after_module_initialization () = allow_dump_override := true
end
