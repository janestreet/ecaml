open Core
open Import
include Sexp

let to_buffer_elisp ~buf sexp =
  let rec loop = function
    | Atom str ->
      Core.Buffer.add_char buf '"';
      String.iter str ~f:(fun c ->
        (match c with
         | '\"' | '\\' -> Core.Buffer.add_char buf '\\'
         | _ -> ());
        Core.Buffer.add_char buf c);
      Core.Buffer.add_char buf '"'
    | List sexps ->
      Core.Buffer.add_char buf '(';
      List.iter sexps ~f:loop;
      Core.Buffer.add_char buf ')'
  in
  loop sexp
;;

let to_form_elisp sexp =
  (* Sexps serialized via this are almost always very large. *)
  let buf = Core.Buffer.create 8192 in
  to_buffer_elisp ~buf sexp;
  Form.read_buffer buf
;;

include Valueable.Make (struct
    type nonrec t = t

    let type_ =
      Value.Type.create
        [%sexp "sexp"]
        [%sexp_of: Sexp.t]
        (fun v ->
          Current_buffer.set_values_temporarily
            Sync
            [ T (Print.level, None); T (Print.length, None) ]
            ~f:(fun () -> Value.prin1_to_string v)
          |> Sexp.of_string)
        (fun sexp -> to_form_elisp sexp |> Form.to_value)
    ;;
  end)

let () =
  Defun.defun
    ("ecaml-sexp-pp-to-string" |> Symbol.intern)
    [%here]
    ~docstring:"Pretty-print SEXP and return the result as a string."
    (Returns Value.Type.string)
    (let%map_open.Defun sexp = required "sexp" t in
     Sexp_pretty.sexp_to_string sexp)
;;

let () =
  Defun.defun
    ("ecaml-sexp-of-string" |> Symbol.intern)
    [%here]
    ~docstring:
      {|Read an OCaml sexp from STRING and return it as a Lisp form.

This function is a thin wrapper around Sexp.of_string.

Naively, one could use `read' for this, but that parses some things differently.
For example, "(foo .?a)" is parsed by `read' as "(foo . ?a)".|}
    (Returns t)
    (let%map_open.Defun string = required "string" string in
     string |> Sexp.of_string)
;;

let () =
  Defun.defun
    ("ecaml-sexp-to-string-mach" |> Symbol.intern)
    [%here]
    ~docstring:"Machine-print SEXP and return the result as a string."
    (Returns Value.Type.string)
    (let%map_open.Defun sexp = required "sexp" t in
     Sexp.to_string_mach sexp)
;;
