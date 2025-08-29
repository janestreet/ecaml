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

let add wrapped_at name symbol =
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
  let t = { wrapped_at; symbol; keymap_var; name; hook } in
  Hashtbl.add_exn t_by_symbol ~key:(symbol |> Symbol.name) ~data:t;
  t
;;

let keymap t = t.keymap_var

let wrap_existing ?here:(wrapped_at = Stdlib.Lexing.dummy_pos) name : (module S) =
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

    let keymap = major_mode.keymap_var

    let enabled_in_current_buffer () =
      Buffer_local.get major_mode_var (Current_buffer0.get ())
      |> Symbol.name
      |> String.( = ) name
    ;;
  end)
;;

let find_or_wrap_existing ?(here = Stdlib.Lexing.dummy_pos) symbol =
  match Hashtbl.find t_by_symbol (symbol |> Symbol.name) with
  | Some t -> t
  | None -> add here Name.Undistinguished symbol
;;

module Fundamental = (val wrap_existing "fundamental-mode")
module Prog = (val wrap_existing "prog-mode")
module Special = (val wrap_existing "special-mode")
module Text = (val wrap_existing "text-mode")
module Tuareg = (val wrap_existing "tuareg-mode")
module Makefile = (val wrap_existing "makefile-mode")
module Lisp_data = (val wrap_existing "lisp-data-mode")
module Scheme = (val wrap_existing "scheme-mode")
module Emacs_lisp = (val wrap_existing "emacs-lisp-mode")
module Asm = (val wrap_existing "asm-mode")
module Python = (val wrap_existing "python-mode")

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
        ~docstring:[%string "Initializer for %{Symbol.name symbol}"]
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
  let m = wrap_existing ~here (symbol |> Symbol.name) in
  let module M = (val m) in
  Dump.keymap_set ~here (keymap M.major_mode) define_keys;
  all_derived_modes := M.major_mode :: !all_derived_modes;
  Option.iter auto_mode ~f:(add_auto_mode ~symbol);
  m
;;

let provided_mode_derived_p =
  Funcall.Wrap.("provided-mode-derived-p" <: Symbol.t @-> Symbol.t @-> return bool)
;;

let is_derived t ~from = provided_mode_derived_p (symbol t) (symbol from)
