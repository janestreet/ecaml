open! Core_kernel
open! Import
open Major_mode_intf

module Q = struct
  include Q

  let define_derived_mode = "define-derived-mode" |> Symbol.intern
  and derived_mode_p = "derived-mode-p" |> Symbol.intern
  and dired_mode = "dired-mode" |> Symbol.intern
  and emacs_lisp_mode = "emacs-lisp-mode" |> Symbol.intern
  and fundamental_mode = "fundamental-mode" |> Symbol.intern
  and lisp_mode = "lisp-mode" |> Symbol.intern
  and makefile_mode = "makefile-mode" |> Symbol.intern
  and prog_mode = "prog-mode" |> Symbol.intern
  and scheme_mode = "scheme-mode" |> Symbol.intern
  and special_mode = "special-mode" |> Symbol.intern
  and text_mode = "text-mode" |> Symbol.intern
  and tuareg_mode = "tuareg-mode" |> Symbol.intern
end

module F = struct
  open Funcall

  let derived_mode_p = Q.derived_mode_p <: Symbol.type_ @-> return bool
end

module Name = struct
  type t = ..
  type t += Undistinguished
end

module Current_buffer = Current_buffer0

type t =
  { wrapped_at : Source_code_position.t
  ; symbol : Symbol.t
  ; keymap_var : Keymap.t Var.t
  ; name : Name.t sexp_opaque
  ; syntax_table_var : Syntax_table.t Var.t
  }
[@@deriving fields, sexp_of]

let equal t1 t2 = Symbol.equal t1.symbol t2.symbol
let compare_name t1 t2 = Symbol.compare_name t1.symbol t2.symbol
let t_by_symbol : t String.Table.t = Hashtbl.create (module String)

let add wrapped_at name symbol =
  let t =
    { wrapped_at
    ; symbol
    ; keymap_var =
        Var.create
          (Symbol.intern (concat [ symbol |> Symbol.name; "-map" ]))
          Keymap.type_
    ; name
    ; syntax_table_var =
        Var.create
          (Symbol.intern (concat [ symbol |> Symbol.name; "-syntax-table" ]))
          Syntax_table.type_
    }
  in
  Hashtbl.add_exn t_by_symbol ~key:(symbol |> Symbol.name) ~data:t;
  t
;;

module type S = S with type t := t and type name := Name.t

let wrap_existing wrapped_at symbol =
  ( module struct
    type Name.t += Major_mode

    let major_mode =
      match Hashtbl.find t_by_symbol (symbol |> Symbol.name) with
      | None -> add wrapped_at Major_mode symbol
      | Some t ->
        raise_s
          [%message
            "Already associated with a name."
              (symbol : Symbol.t)
              (wrapped_at : Source_code_position.t)
              ~previous_def:(t : t)]
    ;;
  end
  : S )
;;

let find_or_wrap_existing here symbol =
  match Hashtbl.find t_by_symbol (symbol |> Symbol.name) with
  | Some t -> t
  | None -> add here Name.Undistinguished symbol
;;

let keymap t = Current_buffer.value_exn t.keymap_var
let keymap_var t = t.keymap_var
let syntax_table t = Current_buffer.value_exn t.syntax_table_var

module Fundamental = (val wrap_existing [%here] Q.fundamental_mode)
module Prog = (val wrap_existing [%here] Q.prog_mode)
module Special = (val wrap_existing [%here] Q.special_mode)
module Text = (val wrap_existing [%here] Q.text_mode)
module Dired = (val wrap_existing [%here] Q.dired_mode)
module Tuareg = (val wrap_existing [%here] Q.tuareg_mode)
module Makefile = (val wrap_existing [%here] Q.makefile_mode)
module Lisp = (val wrap_existing [%here] Q.lisp_mode)
module Scheme = (val wrap_existing [%here] Q.scheme_mode)
module Emacs_lisp = (val wrap_existing [%here] Q.emacs_lisp_mode)

let all_derived_modes = ref []

module For_testing = struct
  let all_derived_modes () = !all_derived_modes |> List.sort ~compare:compare_name
end

let define_derived_mode
      symbol
      here
      ~docstring
      ?(define_keys = [])
      ~mode_line
      ?parent
      ?(initialize = fun () -> ())
      ()
  =
  Form.eval_i
    (Form.list
       [ Q.define_derived_mode |> Form.symbol
       ; symbol |> Form.symbol
       ; (match parent with
          | None -> Form.nil
          | Some t -> Field.get Fields.symbol t |> Form.symbol)
       ; mode_line |> Form.string
       ; docstring |> String.strip |> Form.string
       ; Form.list
           [ Q.funcall |> Form.symbol
           ; Form.quote
               (Function.create here ~args:[] (fun _ ->
                  initialize ();
                  Value.nil)
                |> Function.to_value)
           ]
       ]);
  Load_history.add_entry here (Fun symbol);
  List.iter [ "abbrev-table"; "hook"; "map"; "syntax-table" ] ~f:(fun suffix ->
    Load_history.add_entry
      here
      (Var (concat [ symbol |> Symbol.name; "-"; suffix ] |> Symbol.intern)));
  let m = wrap_existing here symbol in
  let module M = (val m) in
  let the_keymap = keymap M.major_mode in
  List.iter define_keys ~f:(fun (keys, symbol) ->
    Keymap.define_key the_keymap (Key_sequence.create_exn keys) (Symbol symbol));
  all_derived_modes := M.major_mode :: !all_derived_modes;
  m
;;

let major_mode_var =
  Buffer_local.wrap_existing ("major-mode" |> Symbol.intern) Symbol.type_
;;

let is_derived t ~from =
  Current_buffer0.(set_value_temporarily (major_mode_var |> Buffer_local.var) (symbol t))
    ~f:(fun () -> F.derived_mode_p (symbol from))
;;
