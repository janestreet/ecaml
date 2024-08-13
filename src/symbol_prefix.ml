open! Core
open! Import0
include Symbol_prefix_intf

type t = string [@@deriving sexp_of]

module type S = S with type t := t

let create elisp_name =
  (module struct
    let elisp_name = elisp_name
    let symbol_prefix = elisp_name ^ "-"

    let all_commands =
      Lazy.from_fun (fun () ->
        let result = ref [] in
        Obarray.iter Obarray.standard ~f:(fun symbol ->
          let name = Symbol.name symbol in
          if Value.is_command (symbol |> Symbol.to_value)
             && String.is_prefix ~prefix:symbol_prefix name
          then result := symbol :: !result);
        List.sort !result ~compare:(fun a b ->
          Comparable.lift [%compare: string] ~f:Symbol.name a b))
    ;;

    let prefixed_symbol_name suffix =
      match suffix with
      | "" -> elisp_name
      | _ -> symbol_prefix ^ suffix
    ;;

    let symbol suffix =
      if Lazy.is_val all_commands
      then raise_s [%message "[symbol] called after [all_commands] was forced."];
      prefixed_symbol_name suffix |> Symbol.intern
    ;;
  end : S)
;;

let extend t suffix = create (t ^ suffix)
