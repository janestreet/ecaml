open! Core_kernel
open! Import

type t
  =  ?ocaml_name:string
  -> string
  -> Type.t list
  -> Type.t
  -> unit

let stub_prefix = "ecaml__"
let stub name = stub_prefix ^ name

let symbol_to_ocaml symbol =
  String.map symbol ~f:(
    function
    | 'a'..'z'
    | '0'..'9' as c -> c
    | 'A'..'Z' as c -> Char.lowercase c
    | _ -> '_')
;;

let ocaml_name_arg ocaml_name symbol =
  match ocaml_name with
  | Some n -> n
  | None -> symbol_to_ocaml symbol
;;

let ocaml_type_name (t : Type.t) =
  match t with
  | Int    -> "int"
  | Float  -> "float"
  | Bool   -> "bool"
  | Value  -> "Value.t"
  | Ignore -> "unit"
;;

let emacs_of_ocaml (t : Type.t) ocaml_value =
  match t with
  | Int    -> sprintf {|env->make_integer(env, Long_val(%s))|} ocaml_value
  | Float  -> sprintf {|env->make_float(env, Double_val(%s))|} ocaml_value
  | Bool   -> sprintf {|(Bool_val(%s) ? ecaml_emacs_t() : emacs_nil)|} ocaml_value
  | Value  -> sprintf {|emacs_of_ocaml(env, %s)|} ocaml_value
  | Ignore -> raise_s [%message "cannot use [Ignore] as an input type"]
;;

let ocaml_of_emacs (t : Type.t) =
  match t with
  | Int    -> {|Val_long(env->extract_integer(env, emacs_ret))|}
  | Float  -> {|caml_copy_double(env->extract_float(env, emacs_ret))|}
  | Bool   -> {|Val_bool(env->is_not_nil(env, emacs_ret))|}
  | Value  -> {|ocaml_of_emacs(env, emacs_ret)|}
  | Ignore -> {|Val_unit|}
;;


let ocaml ?ocaml_name symbol t_input t_return =
  let ocaml_name = ocaml_name_arg ocaml_name symbol in
  let stub_name = stub ocaml_name in
  let t_return = ocaml_type_name t_return in
  let param_types, param_names =
    if List.is_empty t_input
    then "unit -> ", " ()"
    else (
      let param_types =
        t_input
        |> List.map ~f:(fun t -> ocaml_type_name t ^ " -> ")
        |> String.concat
      in
      let param_names =
        List.init (List.length t_input) ~f:(fun i -> " a" ^ Int.to_string i)
        |> String.concat
      in
      param_types, param_names)
  in
  print_endline (
    concat
      [ "external ";ocaml_name;" : ";param_types; t_return;" = \"";stub_name;"\"\n"
      ; "let ";ocaml_name;"";param_names;" =\n"
      ; "  let r = ";ocaml_name;"";param_names;" in\n"
      ; "  Value.Expert.raise_if_emacs_signaled ();\n"
      ; "  r\n"
      ; ";;\n"]);
;;

let c () =
  print_endline "\
#include <assert.h>

#include <caml/memory.h>

#include \"emacs-module.h\"
#include \"ecaml_stubs.h\"
";
  fun ?ocaml_name symbol t_input t_return ->
    let ocaml_name = ocaml_name_arg ocaml_name symbol in
    let stub_name = stub ocaml_name in
    let is_nullary = List.is_empty t_input in
    let c_params =
      if is_nullary
      then "value unit"
      else (
        t_input
        |> List.mapi ~f:(fun i _ -> "value a" ^ string_of_int i)
        |> String.concat ~sep:", ")
    in
    let caml_params =
      let params =
        if is_nullary
        then [ "unit" ]
        else List.mapi t_input ~f:(fun i _ -> sprintf "a%d" i)
      in
      List.mapi (List.chunks_of params ~length:5) ~f:(fun i group ->
        sprintf "  CAML%sparam%d(%s);\n"
          (if i = 0 then "" else "x")
          (List.length group)
          (String.concat ~sep:", " group))
      |> String.concat
    in
    let nargs = List.length t_input |> Int.to_string in
    let convert_args =
      if is_nullary
      then ""
      else (
        concat
          [ "  emacs_value emacs_args[";nargs;"];\n"
          ; t_input
            |> List.mapi ~f:(fun index t ->
              sprintf "  emacs_args[%d] = %s;\n"
                index
                (emacs_of_ocaml t (concat [ "a"; index |> Int.to_string ])))
            |> String.concat])
    in
    let args =
      if is_nullary
      then "NULL"
      else "emacs_args"
    in
    print_endline (
      concat
        [ "CAMLprim value ";stub_name;"(";c_params;")\n"
        ; "{\n"
        ; caml_params
        ; "  CAMLlocal1(ret);\n"
        ; "  emacs_env* env = ecaml_active_env_or_die();\n"
        ; if is_nullary
          then "  assert(unit == Val_unit);\n"
          else ""
        ; "  static emacs_value emacs_fun = NULL;\n"
        ; "  ecaml_cache_symbol_and_keep_alive(env, &emacs_fun, \"";symbol;"\");\n"
        ; convert_args
        ; "  emacs_value emacs_ret = env->funcall(env, emacs_fun, ";nargs;", ";args;");\n"
        ; "  ret = ";ocaml_of_emacs t_return;";\n"
        ; "  CAMLreturn(ret);\n"
        ; "}\n" ]);
;;
