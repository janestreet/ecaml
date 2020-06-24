open! Core_kernel
open! Import
open! Async_kernel
open Major_mode_intf
module Hook = Hook0

module Q = struct
  include Q

  let define_derived_mode = "define-derived-mode" |> Symbol.intern
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
  ; name : (Name.t[@sexp.opaque])
  ; hook : Hook.normal Hook.t
  ; syntax_table_var : Syntax_table.t Var.t
  }
[@@deriving fields, sexp_of]

let equal t1 t2 = Symbol.equal t1.symbol t2.symbol
let compare_name t1 t2 = Symbol.compare_name t1.symbol t2.symbol
let t_by_symbol : t String.Table.t = Hashtbl.create (module String)

module Compare_by_name = struct
  type nonrec t = t [@@deriving sexp_of]

  let to_string = symbol >> Symbol.name
  let hash = to_string >> String.hash
  let hash_fold_t state t = String.hash_fold_t state (to_string t)
  let equal t1 t2 = String.equal (to_string t1) (to_string t2)
  let compare = Comparable.lift String.compare ~f:to_string
end

let major_mode_var = Buffer_local.Wrap.("major-mode" <: Symbol.t)

let change_to t ~in_:buffer =
  Value.Private.run_outside_async [%here] ~allowed_in_background:true (fun () ->
    Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
      Funcall.Wrap.(symbol t |> Symbol.name <: nullary @-> return nil) ()))
;;

let add wrapped_at name symbol =
  let t =
    { wrapped_at
    ; symbol
    ; keymap_var = Var.Wrap.(concat [ symbol |> Symbol.name; "-map" ] <: Keymap.t)
    ; name
    ; hook =
        Hook.create
          (Symbol.intern (concat [ symbol |> Symbol.name; "-hook" ]))
          ~hook_type:Normal
    ; syntax_table_var =
        Var.Wrap.(concat [ symbol |> Symbol.name; "-syntax-table" ] <: Syntax_table.t)
    }
  in
  Hashtbl.add_exn t_by_symbol ~key:(symbol |> Symbol.name) ~data:t;
  t
;;

module type S = S with type t := t and type name := Name.t

let wrap_existing name wrapped_at =
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

    let enabled_in_current_buffer () =
      Buffer_local.get major_mode_var (Current_buffer0.get ())
      |> Symbol.name
      |> String.( = ) name
    ;;
  end : S)
;;

let find_or_wrap_existing here symbol =
  match Hashtbl.find t_by_symbol (symbol |> Symbol.name) with
  | Some t -> t
  | None -> add here Name.Undistinguished symbol
;;

let keymap t = Current_buffer.value_exn t.keymap_var
let keymap_var t = t.keymap_var
let syntax_table t = Current_buffer.value_exn t.syntax_table_var

module Fundamental = (val wrap_existing "fundamental-mode" [%here])
module Prog = (val wrap_existing "prog-mode" [%here])
module Special = (val wrap_existing "special-mode" [%here])
module Text = (val wrap_existing "text-mode" [%here])
module Dired = (val wrap_existing "dired-mode" [%here])
module Tuareg = (val wrap_existing "tuareg-mode" [%here])
module Makefile = (val wrap_existing "makefile-mode" [%here])
module Lisp = (val wrap_existing "lisp-mode" [%here])
module Scheme = (val wrap_existing "scheme-mode" [%here])
module Emacs_lisp = (val wrap_existing "emacs-lisp-mode" [%here])

let all_derived_modes = ref []

module For_testing = struct
  let all_derived_modes () = !all_derived_modes |> List.sort ~compare:compare_name
end

let define_derived_mode
      (type a)
      symbol
      here
      ~docstring
      ?(define_keys = [])
      ~mode_line
      ?parent
      ?(initialize : ((unit, a) Defun.Returns.t * (unit -> a)) option)
      ()
  =
  let initialize_fn =
    match initialize with
    | None -> Defun.lambda_nullary_nil here ident
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
       ; docstring |> String.strip |> Form.string
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
  let the_keymap = keymap M.major_mode in
  List.iter define_keys ~f:(fun (keys, symbol) ->
    Keymap.define_key the_keymap (Key_sequence.create_exn keys) (Symbol symbol));
  all_derived_modes := M.major_mode :: !all_derived_modes;
  m
;;

let derived_mode_p = Funcall.Wrap.("derived-mode-p" <: Symbol.t @-> return bool)

let is_derived t ~from =
  Current_buffer0.(
    set_value_temporarily Sync (major_mode_var |> Buffer_local.var) (symbol t))
    ~f:(fun () -> derived_mode_p (symbol from))
;;
