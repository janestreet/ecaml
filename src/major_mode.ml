open! Core
open! Import
open! Async_kernel
include Major_mode_intf
module Hook = Hook0

module Q = struct
  include Q

  let define_derived_mode = "define-derived-mode" |> Symbol.intern
end

module Current_buffer = Current_buffer0

type t =
  { wrapped_at : Source_code_position.t
  ; symbol : Symbol.t
  ; keymap_var : Keymap.t Var.t
  ; name : (Name.t[@sexp.opaque])
  ; hook : (Hook.normal, unit) Hook.t Or_error.t
  ; syntax_table_var : Syntax_table.t Var.t
  }
[@@deriving fields ~getters ~fields, sexp_of]

let equal t1 t2 = Symbol.equal t1.symbol t2.symbol
let compare_name t1 t2 = Symbol.compare_name t1.symbol t2.symbol
let t_by_symbol : t String.Table.t = Hashtbl.create (module String)

include Intf (struct
  type nonrec t = t
end)

module Compare_by_name = struct
  type nonrec t = t [@@deriving sexp_of]

  let to_string = symbol >> Symbol.name
  let hash = to_string >> String.hash
  let hash_fold_t state t = String.hash_fold_t state (to_string t)
  let equal t1 t2 = String.equal (to_string t1) (to_string t2)
  let compare a b = Comparable.lift String.compare ~f:to_string a b
end

let major_mode_var = Buffer_local.Wrap.("major-mode" <: Symbol.t)

let change_to t ~in_:buffer =
  Value.Private.run_outside_async [%here] ~allowed_in_background:true (fun () ->
    Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
      Funcall.Wrap.(symbol t |> Symbol.name <: nullary @-> return nil) ()))
;;

let add wrapped_at name symbol =
  let hook =
    let fundamental_mode = Symbol.intern "fundamental-mode" in
    match [%compare.equal: Symbol.Compare_name.t] symbol fundamental_mode with
    | false -> Ok Hook.Wrap.(concat [ symbol |> Symbol.name; "-hook" ] <: Normal_hook)
    | true ->
      error_s
        [%message
          {|fundamental-mode has no mode hook. [(Info-goto-node "(elisp) Major Modes")]|}]
  in
  let keymap_var = Var.Wrap.(concat [ symbol |> Symbol.name; "-map" ] <: Keymap.t) in
  let syntax_table_var =
    Var.Wrap.(concat [ symbol |> Symbol.name; "-syntax-table" ] <: Syntax_table.t)
  in
  let t = { wrapped_at; symbol; keymap_var; name; hook; syntax_table_var } in
  Hashtbl.add_exn t_by_symbol ~key:(symbol |> Symbol.name) ~data:t;
  t
;;

let keymap t = Current_buffer.value_exn t.keymap_var
let keymap_var t = t.keymap_var
let syntax_table t = Current_buffer.value_exn t.syntax_table_var

let wrap_existing_with_lazy_keymap name wrapped_at : (module S_with_lazy_keymap) =
  (module struct
    type Name.t += Major_mode

    let major_mode =
      match Hashtbl.find t_by_symbol name with
      | None -> add wrapped_at Major_mode (name |> Symbol.intern)
      | Some t ->
        raise_s
          [%message
            "Already associated with a name."
              (name : string)
              (wrapped_at : Source_code_position.t)
              ~previous_def:(t : t)]
    ;;

    let keymap =
      lazy
        (try keymap major_mode with
         | exn ->
           raise_s
             [%message
               "Major mode's keymap doesn't exist"
                 (name : string)
                 (wrapped_at : Source_code_position.t)
                 (exn : exn)])
    ;;

    let enabled_in_current_buffer () =
      Buffer_local.get major_mode_var (Current_buffer0.get ())
      |> Symbol.name
      |> String.( = ) name
    ;;
  end)
;;

let wrap_existing name wrapped_at : (module S) =
  (module struct
    include (val wrap_existing_with_lazy_keymap name wrapped_at)

    let keymap = force keymap
  end)
;;

let find_or_wrap_existing here symbol =
  match Hashtbl.find t_by_symbol (symbol |> Symbol.name) with
  | Some t -> t
  | None -> add here Name.Undistinguished symbol
;;

module Fundamental = (val wrap_existing_with_lazy_keymap "fundamental-mode" [%here])
module Prog = (val wrap_existing "prog-mode" [%here])
module Special = (val wrap_existing "special-mode" [%here])
module Text = (val wrap_existing "text-mode" [%here])
module Dired = (val wrap_existing_with_lazy_keymap "dired-mode" [%here])
module Tuareg = (val wrap_existing_with_lazy_keymap "tuareg-mode" [%here])
module Makefile = (val wrap_existing_with_lazy_keymap "makefile-mode" [%here])
module Lisp_data = (val wrap_existing "lisp-data-mode" [%here])
module Scheme = (val wrap_existing_with_lazy_keymap "scheme-mode" [%here])
module Emacs_lisp = (val wrap_existing "emacs-lisp-mode" [%here])
module Asm = (val wrap_existing_with_lazy_keymap "asm-mode" [%here])
module Python = (val wrap_existing_with_lazy_keymap "python-mode" [%here])

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
      Defun.define_obsolete_alias symbol here ~alias_of:new_ ~since ();
      List.iter
        [ "abbrev-table"; "hook"; "map"; "syntax-table" ]
        ~f:
          (let old_prefix = Symbol.name symbol in
           let new_prefix = Symbol.name new_ in
           fun suffix ->
             Defvar.define_obsolete_alias
               ([%string "%{old_prefix}-%{suffix}"] |> Symbol.intern)
               here
               ~alias_of:([%string "%{new_prefix}-%{suffix}"] |> Symbol.intern)
               ~since
               ());
      new_
  in
  let docstring = docstring |> String.strip in
  require_nonempty_docstring here ~docstring;
  let initialize_fn =
    match initialize with
    | None -> Defun.lambda_nullary_nil here Fn.id
    | Some (returns, f) -> Defun.lambda_nullary here returns f
  in
  Form.Blocking.eval_i
    (Form.list
       [ Q.define_derived_mode |> Form.symbol
       ; symbol |> Form.symbol
       ; (match parent with
          | None -> Form.nil
          | Some t -> Field.get Fields.symbol t |> Form.symbol)
       ; mode_line |> Form.string
       ; docstring |> Form.string
       ; Form.list
           [ Q.funcall |> Form.symbol; Form.quote (initialize_fn |> Function.to_value) ]
       ]);
  Load_history.add_entry here (Fun symbol);
  List.iter [ "abbrev-table"; "hook"; "map"; "syntax-table" ] ~f:(fun suffix ->
    Load_history.add_entry
      here
      (Var (concat [ symbol |> Symbol.name; "-"; suffix ] |> Symbol.intern)));
  let m = wrap_existing (symbol |> Symbol.name) here in
  let module M = (val m) in
  List.iter define_keys ~f:(fun (keys, symbol) ->
    Keymap.define_key M.keymap (Key_sequence.create_exn keys) (Symbol symbol));
  all_derived_modes := M.major_mode :: !all_derived_modes;
  Option.iter auto_mode ~f:(add_auto_mode ~symbol);
  m
;;

let derived_mode_p = Funcall.Wrap.("derived-mode-p" <: Symbol.t @-> return bool)

let is_derived t ~from =
  Current_buffer0.set_value_temporarily
    Sync
    (major_mode_var |> Buffer_local.var)
    (symbol t)
    ~f:(fun () -> derived_mode_p (symbol from))
;;
