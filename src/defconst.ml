open! Core
open! Import

module Q = struct
  let defconst = "defconst" |> Symbol.intern
end

let defconst_i symbol here ~docstring ~(type_ : _ Value.Type.t) ~value =
  Load_history.add_entry here (Var symbol);
  Dump.eval_and_dump ~here (fun () ->
    let docstring = docstring |> String.strip in
    require_nonempty_docstring here ~docstring;
    Form.list
      [ Form.symbol Q.defconst
      ; Form.symbol symbol
      ; Form.quote (Value.Type.to_value type_ value)
      ; Form.string docstring
      ])
;;

let defconst symbol here ~docstring ~type_ ~value =
  defconst_i symbol here ~docstring ~type_ ~value;
  Var.create symbol type_
;;
