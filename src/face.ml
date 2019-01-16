open! Core_kernel
open! Import0
include Face0

let defface here name specs ~docstring =
  Form.(
    eval_i
      (list
         [ symbol Q.defface
         ; symbol (Symbol.intern name)
         ; quote (specs_to_value specs)
         ; string docstring
         ]));
  let t = of_name name in
  Load_history.add_entry here (Face t);
  t
;;
