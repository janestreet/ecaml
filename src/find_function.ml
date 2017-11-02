open! Core_kernel
open! Import

let find_function function_ =
  Symbol.funcall1_i Q.find_function (function_ |> Symbol.to_value);
;;

let find_ocaml ~library ~symbol ~type_ =
  Buffer.find_file_noselect library,
  (Load_history.location_exn symbol type_).pos_cnum + 1 |> Position.of_int_exn
;;

let advise_for_ocaml () =
  Feature.require ("find-func" |> Symbol.intern);
  let advice_name =
    "find-function-search-ocaml" |> Symbol.intern |> Advice.Name.of_symbol
  in
  let for_function = "find-function-search-for-symbol" |> Symbol.intern in
  Advice.defadvice [%here]
    ~advice_name
    ~docstring:"Handle Ecaml files."
    ~for_function
    (fun ~args ~inner ->
       match args with
       (* [find-function-search-for-symbol] is used by both [find-function] and
          [find-variable], so [symbol] can be a function or a variable. *)
       | [ symbol; type_; library ] ->
         (match Value.to_utf8_bytes_exn library with
          | exception _ -> inner args
          | library ->
            if not (String.is_suffix library ~suffix:".ml")
            then inner args
            else (
              let type_ : Load_history.Type.t =
                if Value.is_nil type_
                then Fun
                else if Value.eq type_ (Q.defvar :> Value.t)
                then Var
                else raise_s [%message "unrecognized type" ~_:(type_ : Value.t)]
              in
              let buffer, position =
                find_ocaml
                  ~library
                  ~symbol:(symbol |> Symbol.of_value_exn)
                  ~type_
              in
              Value.cons
                (buffer   |> Buffer.to_value)
                (position |> Position.to_value)))
       | _ -> raise_s [%message
                "unexpected number of arguments" (for_function : Symbol.t)]);
  Advice.activate_function for_function;
;;

let initialize () =
  advise_for_ocaml ();
;;
