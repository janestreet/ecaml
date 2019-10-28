open! Core_kernel
open! Import

module Q = struct
  let defconst = "defconst" |> Symbol.intern
end

let all_defconst_symbols = ref []

module Private = struct
  let all_defconst_symbols () =
    !all_defconst_symbols |> List.sort ~compare:Symbol.compare_name
  ;;
end

let defconst_i symbol here ~docstring ~(type_ : _ Value.Type.t) ~value =
  all_defconst_symbols := symbol :: !all_defconst_symbols;
  Load_history.add_entry here (Var symbol);
  Form.list
    [ Form.symbol Q.defconst
    ; Form.symbol symbol
    ; Form.quote (Value.Type.to_value type_ value)
    ; Form.string (docstring |> String.strip)
    ]
  |> Form.Blocking.eval_i
;;

let defconst symbol here ~docstring ~type_ ~value =
  defconst_i symbol here ~docstring ~type_ ~value;
  Var.create symbol type_
;;
