open! Core_kernel
open! Import0
include Face0

let defface name here ~docstring ~customization_group specs =
  let docstring = docstring |> String.strip in
  require_nonempty_docstring here ~docstring;
  Form.(
    Blocking.eval_i
      (list
         [ symbol Q.defface
         ; symbol (Symbol.intern name)
         ; quote (specs_to_value specs)
         ; string docstring
         ; Q.K.group |> symbol
         ; customization_group |> Customization.Group.to_value |> quote
         ]));
  let t = of_name name in
  Load_history.add_entry here (Face t);
  t
;;
