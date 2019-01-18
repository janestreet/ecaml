open! Core_kernel
open! Import

module Q = struct
  include Q

  let defvaralias = "defvaralias" |> Symbol.intern
end

module F = struct
  open! Funcall
  open! Value.Type

  let defvaralias =
    Q.defvaralias <: Symbol.type_ @-> Symbol.type_ @-> option string @-> return nil
  ;;
end

let all_defvar_symbols = ref []

module Private = struct
  let all_defvar_symbols () =
    !all_defvar_symbols |> List.sort ~compare:Symbol.compare_name
  ;;
end

let add symbol here =
  all_defvar_symbols := symbol :: !all_defvar_symbols;
  Load_history.add_entry here (Var symbol)
;;

let defvar symbol here ~docstring ~type_ ~initial_value () =
  ignore
    ( Form.eval
        ([ Q.defvar |> Symbol.to_value
         ; symbol |> Symbol.to_value
         ; Value.list
             [ Symbol.to_value Q.quote; initial_value |> Value.Type.to_value type_ ]
         ; docstring |> String.strip |> Value.of_utf8_bytes
         ]
         |> Value.list
         |> Form.of_value_exn)
      : Value.t );
  add symbol here;
  Var.create symbol type_
;;

let defvaralias symbol here ?docstring ~alias_of () =
  F.defvaralias symbol alias_of docstring;
  add symbol here
;;

let define_obsolete_alias obsolete here ?docstring ~alias_of ~since () =
  defvaralias obsolete here ?docstring ~alias_of ();
  Obsolete.make_variable_obsolete obsolete ~current:alias_of ~since
;;
