open! Core
open! Import

let all_defvar_symbols = ref []

module Private = struct
  let all_defvar_symbols () =
    !all_defvar_symbols |> List.sort ~compare:Symbol.compare_name
  ;;
end

let add_to_load_history symbol here =
  all_defvar_symbols := symbol :: !all_defvar_symbols;
  Load_history.add_entry here (Var symbol)
;;

let defvaralias =
  Funcall.Wrap.("defvaralias" <: Symbol.t @-> Symbol.t @-> nil_or string @-> return nil)
;;

let defvaralias symbol here ?docstring ~alias_of () =
  defvaralias symbol alias_of docstring;
  add_to_load_history symbol here
;;

let define_obsolete_alias obsolete here ?docstring ~alias_of ~since () =
  defvaralias obsolete here ?docstring ~alias_of ();
  Obsolete.make_variable_obsolete obsolete ~current:(Some alias_of) ~since
;;

let defvar
  symbol
  here
  ~docstring
  ~type_
  ~initial_value
  ?(include_in_all_defvar_symbols = true)
  ()
  =
  let symbol =
    match Symbol.Automatic_migration.migrate ~old:symbol with
    | None -> symbol
    | Some { new_; since } ->
      define_obsolete_alias symbol here ~alias_of:new_ ~since ();
      new_
  in
  let docstring = docstring |> String.strip in
  require_nonempty_docstring here ~docstring;
  ignore
    (Form.Blocking.eval
       ([ Q.defvar |> Symbol.to_value
        ; symbol |> Symbol.to_value
        ; Value.list
            [ Symbol.to_value Q.quote; initial_value |> Value.Type.to_value type_ ]
        ; docstring |> Value.of_utf8_bytes
        ]
        |> Value.list
        |> Form.of_value_exn)
     : Value.t);
  if include_in_all_defvar_symbols then add_to_load_history symbol here;
  Var.create symbol type_
;;
