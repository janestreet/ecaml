open! Core_kernel
open! Import

module Q = struct
  include Q

  let find_function = "find-function" |> Symbol.intern
end

let find_function function_ =
  Symbol.funcall1_i Q.find_function (function_ |> Symbol.to_value)
;;

let find_ocaml ~library ~symbol ~type_ =
  ( Buffer.find_file_noselect library
  , Some ((Load_history.location_exn symbol type_).pos_cnum + 1 |> Position.of_int_exn) )
;;

let advise_for_ocaml () =
  Feature.require ("find-func" |> Symbol.intern);
  let for_function = "find-function-search-for-symbol" |> Symbol.intern in
  Advice.around_funcall
    ("find-function-search-ocaml" |> Symbol.intern)
    [%here]
    ~for_function
    (let open Funcall in
     let open Value.Type in
     Symbol.type_
     @-> Load_history.Type.type_
     @-> string
     @-> return (tuple Buffer.type_ (option Position.type_)))
    (fun inner symbol type_ library ->
       (* [find-function-search-for-symbol] is used by both [find-function] and
          [find-variable], so [symbol] can be a function or a variable. *)
       if not (String.is_suffix library ~suffix:".ml")
       then inner symbol type_ library
       else find_ocaml ~library ~symbol ~type_)
;;

let initialize () = advise_for_ocaml ()
