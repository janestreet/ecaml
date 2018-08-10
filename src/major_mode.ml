open! Core_kernel
open! Import

module Q = struct
  include Q

  let define_derived_mode = "define-derived-mode" |> Symbol.intern
  and fundamental_mode = "fundamental-mode" |> Symbol.intern
  and prog_mode = "prog-mode" |> Symbol.intern
  and special_mode = "special-mode" |> Symbol.intern
  and text_mode = "text-mode" |> Symbol.intern
  ;;
end

module Name = struct
  type t = ..

  type t += Undistinguished

  let distinguished_values : (t * Source_code_position.t) String.Table.t =
    String.Table.create ()
  ;;

  let add_or_get_distinguished_value t here ~change_command =
    let key = Symbol.name change_command in
    match t with
    | None ->
      (match Hashtbl.find distinguished_values key with
       | Some (t, _) -> t
       | None -> Undistinguished)
    | Some t ->
      (match Hashtbl.find distinguished_values key with
       | Some (_, previous_def) ->
         raise_s
           [%message
             "Already associated with a name."
               (change_command : Symbol.t)
               (here : Source_code_position.t)
               (previous_def : Source_code_position.t)]
       | None ->
         Hashtbl.set distinguished_values ~key ~data:(t, here);
         t)
  ;;
end

module Current_buffer = Current_buffer0

type t =
  { change_command : Symbol.t
  ; keymap_var : Keymap.t Var.t
  ; name : Name.t sexp_opaque
  ; syntax_table_var : Syntax_table.t Var.t
  }
[@@deriving fields, sexp_of]

let equal t1 t2 = Symbol.equal t1.change_command t2.change_command

let create here name ~change_command =
  { change_command
  ; keymap_var =
      Var.create
        (Symbol.intern (concat [ change_command |> Symbol.name; "-map" ]))
        Keymap.type_
  ; name = Name.add_or_get_distinguished_value name here ~change_command
  ; syntax_table_var =
      Var.create
        (Symbol.intern (concat [ change_command |> Symbol.name; "-syntax-table" ]))
        Syntax_table.type_
  }
;;

let keymap t = Current_buffer.value_exn t.keymap_var

let keymap_var t = t.keymap_var

let syntax_table t = Current_buffer.value_exn t.syntax_table_var

type Name.t += Fundamental

let fundamental = create [%here] (Some Fundamental) ~change_command:Q.fundamental_mode

type Name.t += Prog

let prog = create [%here] (Some Prog) ~change_command:Q.prog_mode

type Name.t += Special

let special = create [%here] (Some Special) ~change_command:Q.special_mode

type Name.t += Text

let text = create [%here] (Some Text) ~change_command:Q.text_mode

module For_testing = struct
  let derived_modes = ref []

  let finish_deriving_modes = lazy !derived_modes

  let log_derived_mode t =
    match Lazy.is_val finish_deriving_modes with
    | true ->
      raise_s
        [%message "Cannot [define_derived_mode] after [force finish_deriving_modes]."]
    | false -> derived_modes := t :: !derived_modes
  ;;
end

let define_derived_mode
      ?(define_keys=[])
      ?parent
      here
      name
      ~change_command
      ~docstring
      ~initialize
      ~mode_line
  =
  Form.eval_i
    (Form.list
       [ Q.define_derived_mode |> Form.symbol
       ; change_command |> Form.symbol
       ; (match parent with
          | None -> Form.nil
          | Some t -> Field.get Fields.change_command t |> Form.symbol)
       ; mode_line |> Form.string
       ; docstring |> Form.string
       ; Form.list
           [ Q.funcall |> Form.symbol
           ; Form.quote
               (Function.create here ~args:[] (fun _ ->
                  initialize ();
                  Value.nil)
                |> Function.to_value)
           ]
       ]);
  Load_history.add_entry here (Fun change_command);
  List.iter [ "abbrev-table"; "hook"; "map"; "syntax-table" ] ~f:(fun suffix ->
    Load_history.add_entry
      here
      (Var (concat [ change_command |> Symbol.name; "-"; suffix ] |> Symbol.intern)));
  let t = create here (Some name) ~change_command in
  let the_keymap = keymap t in
  List.iter define_keys ~f:(fun (keys, symbol) ->
    Keymap.define_key the_keymap (Key_sequence.create_exn keys) (Symbol symbol));
  For_testing.log_derived_mode t;
  t
;;
