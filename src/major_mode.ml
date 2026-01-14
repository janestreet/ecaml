open! Core
open! Import
open! Async_kernel
module Hook = Hook0

module Q = struct
  include Q

  let define_derived_mode = "define-derived-mode" |> Symbol.intern
end

module Auto_mode = struct
  type t =
    | If_filename_matches of Regexp.t
    | If_filename_matches_then_delete_suffix_and_recur of Regexp.t
end

module Current_buffer = Current_buffer0

type t =
  { wrapped_at : Source_code_position.t
  ; symbol : Symbol.t
  ; keymap_var : Keymap.t Var.t
  ; hook : (Hook.normal, unit) Hook.t Or_error.t
  }
[@@deriving fields ~getters ~fields, sexp_of]

let equal t1 t2 = Symbol.equal t1.symbol t2.symbol
let compare_name t1 t2 = Symbol.compare_name t1.symbol t2.symbol
let t_by_symbol : t String.Table.t = Hashtbl.create (module String)
let major_mode_var = Buffer_local.Wrap.("major-mode" <: Symbol.t)

module Blocking = struct
  let change_to t ~in_:buffer =
    Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
      Funcall.Wrap.(symbol t |> Symbol.name <: nullary @-> return nil) ())
  ;;
end

let change_to t ~in_ =
  Value.Private.run_outside_async ~allowed_in_background:true (fun () ->
    Blocking.change_to t ~in_)
;;

let add wrapped_at symbol =
  let hook =
    let fundamental_mode = Symbol.intern "fundamental-mode" in
    match [%compare.equal: Symbol.Compare_name.t] symbol fundamental_mode with
    | false -> Ok Hook.Wrap.([%string "%{Symbol.name symbol}-hook"] <: Normal_hook)
    | true ->
      error_s
        [%message
          {|fundamental-mode has no mode hook. [(Info-goto-node "(elisp) Major Modes")]|}]
  in
  let keymap_var = Var.Wrap.([%string "%{Symbol.name symbol}-map"] <: Keymap.t) in
  let t = { wrapped_at; symbol; keymap_var; hook } in
  Hashtbl.add_exn t_by_symbol ~key:(symbol |> Symbol.name) ~data:t;
  t
;;

let keymap t = t.keymap_var

let wrap_existing ~here:(wrapped_at : [%call_pos]) name : t =
  match Hashtbl.find t_by_symbol name with
  | None -> add wrapped_at (name |> Symbol.intern)
  | Some t ->
    raise_s
      [%message
        "Already associated with a name."
          (name : string)
          (wrapped_at : Source_code_position.t)
          ~previous_def:(t : t)]
;;

let find_or_wrap_existing ~(here : [%call_pos]) symbol =
  match Hashtbl.find t_by_symbol (symbol |> Symbol.name) with
  | Some t -> t
  | None -> add here symbol
;;

let fundamental = wrap_existing "fundamental-mode"
let prog = wrap_existing "prog-mode"
let special = wrap_existing "special-mode"
let text = wrap_existing "text-mode"
let tuareg = wrap_existing "tuareg-mode"
let makefile = wrap_existing "makefile-mode"
let lisp_data = wrap_existing "lisp-data-mode"
let scheme = wrap_existing "scheme-mode"
let emacs_lisp = wrap_existing "emacs-lisp-mode"
let asm = wrap_existing "asm-mode"
let python = wrap_existing "python-mode"
let image = wrap_existing "image-mode"
let all_derived_modes = ref []

module For_testing = struct
  let all_derived_modes () = !all_derived_modes |> List.sort ~compare:compare_name
end

let add_auto_mode auto_mode ~symbol =
  let filename_match, delete_suffix_and_recur =
    match (auto_mode : Auto_mode.t) with
    | If_filename_matches regexp -> regexp, false
    | If_filename_matches_then_delete_suffix_and_recur regexp -> regexp, true
  in
  Auto_mode_alist.add
    [ { delete_suffix_and_recur; filename_match; function_ = Some symbol } ]
;;

let define_derived_mode
  (type a)
  ?auto_mode
  symbol
  here
  ~docstring
  ?(define_keys = [])
  ~mode_line
  ?parent
  ?(initialize : ((unit, a) Defun.Returns.t * (unit -> a)) option)
  ()
  =
  let symbol =
    match Symbol.Automatic_migration.migrate ~old:symbol with
    | None -> symbol
    | Some { new_; since } ->
      Defun.define_obsolete_alias symbol ~here ~alias_of:new_ ~since ();
      List.iter
        [ "abbrev-table"; "hook"; "map"; "syntax-table" ]
        ~f:
          (let old_prefix = Symbol.name symbol in
           let new_prefix = Symbol.name new_ in
           fun suffix ->
             Defvar.define_obsolete_alias
               ([%string "%{old_prefix}-%{suffix}"] |> Symbol.intern)
               ~here
               ~alias_of:([%string "%{new_prefix}-%{suffix}"] |> Symbol.intern)
               ~since
               ());
      new_
  in
  let docstring = docstring |> String.strip in
  require_nonempty_docstring here ~docstring;
  let initialize =
    Option.map initialize ~f:(fun (returns, f) ->
      Defun.defun_func
        ([%string "%{Symbol.name symbol}--init"] |> Symbol.intern)
        here
        ~docstring:
          [%string "Initializer for %{Documentation.Special_sequence.symbol symbol}."]
        returns
        (let%map_open.Defun () = return () in
         f ())
      |> Function.to_value
      |> Symbol.of_value_exn)
  in
  Dump.eval_and_dump ~here (fun () ->
    Form.list
      ([ Q.define_derived_mode |> Form.symbol
       ; symbol |> Form.symbol
       ; (match parent with
          | None -> Form.nil
          | Some t -> Field.get Fields.symbol t |> Form.symbol)
       ; mode_line |> Form.string
       ; docstring |> Form.string
       ]
       @
       match initialize with
       | None -> []
       | Some fn -> [ Form.apply fn [] ]));
  Load_history.add_entry here (Fun symbol);
  List.iter [ "abbrev-table"; "hook"; "map"; "syntax-table" ] ~f:(fun suffix ->
    Load_history.add_entry
      here
      (Var ([%string "%{Symbol.name symbol}-%{suffix}"] |> Symbol.intern)));
  let major_mode = wrap_existing ~here (symbol |> Symbol.name) in
  Dump.keymap_set ~here (keymap major_mode) define_keys;
  all_derived_modes := major_mode :: !all_derived_modes;
  Option.iter auto_mode ~f:(add_auto_mode ~symbol);
  major_mode
;;

let provided_mode_derived_p =
  Funcall.Wrap.("provided-mode-derived-p" <: Symbol.t @-> Symbol.t @-> return bool)
;;

let is_derived t ~from = provided_mode_derived_p (symbol t) (symbol from)
