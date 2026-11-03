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

(* [prin1] prints Elisp read syntax, which differs from sexp syntax, so we cannot parse
   its output with [Sexp.of_string]. In particular, [prin1] escapes non-ASCII characters
   in unibyte strings (such as the strings produced by [to_form_elisp] for atoms
   containing UTF-8) as octal [\OOO] escapes, whereas [Sexp.of_string] reads [\ddd]
   escapes as decimal.

   We don't support all of Elisp read syntax, just what [prin1] produces for the values
   [Ecaml_sexp] itself creates: strings, proper lists, and the bare symbol [nil], which is
   how [prin1] prints the empty list. *)
let of_prin1_output string =
  let len = String.length string in
  let pos = ref 0 in
  let[@cold] parse_error message =
    raise_s
      [%message
        "could not parse [prin1] output"
          ~_:(message : string)
          (string : string)
          ~pos:(!pos : int)]
  in
  let peek () = if Int.( < ) !pos len then Some string.[!pos] else None in
  let next () =
    match peek () with
    | Some c ->
      incr pos;
      c
    | None -> parse_error "unexpected end of input"
  in
  let is_whitespace = function
    | ' ' | '\t' | '\n' | '\r' | '\012' -> true
    | _ -> false
  in
  let skip_whitespace () =
    while
      match peek () with
      | Some c -> is_whitespace c
      | None -> false
    do
      incr pos
    done
  in
  let add_string_escape buf =
    match next () with
    | '0' .. '7' as c ->
      (* An octal escape: up to three octal digits encoding a byte. *)
      let octal_digit c = Char.to_int c - Char.to_int '0' in
      let value = ref (octal_digit c) in
      let digits = ref 1 in
      while
        Int.( < ) !digits 3
        &&
        match peek () with
        | Some '0' .. '7' -> true
        | Some _ | None -> false
      do
        value := (!value * 8) + octal_digit (next ());
        incr digits
      done;
      (match Char.of_int !value with
       | Some c -> Core.Buffer.add_char buf c
       | None -> parse_error "octal escape out of range")
    | 'n' -> Core.Buffer.add_char buf '\n'
    | 't' -> Core.Buffer.add_char buf '\t'
    | 'r' -> Core.Buffer.add_char buf '\r'
    | 'f' -> Core.Buffer.add_char buf '\012'
    | 'b' -> Core.Buffer.add_char buf '\b'
    | 'a' -> Core.Buffer.add_char buf '\007'
    | 'v' -> Core.Buffer.add_char buf '\011'
    | 'e' -> Core.Buffer.add_char buf '\027'
    | 's' -> Core.Buffer.add_char buf ' '
    | 'd' -> Core.Buffer.add_char buf '\127'
    | 'x' | 'u' | 'U' | 'N' | '^' -> parse_error "unsupported string escape"
    | c ->
      (* Escaped double quotes, backslashes, and other escaped characters stand for
         themselves. *)
      Core.Buffer.add_char buf c
  in
  let parse_quoted_string () =
    let buf = Core.Buffer.create 16 in
    let rec loop () =
      match next () with
      | '"' -> Core.Buffer.contents buf
      | '\\' ->
        add_string_escape buf;
        loop ()
      | c ->
        Core.Buffer.add_char buf c;
        loop ()
    in
    loop ()
  in
  let parse_bare_atom () =
    let start = !pos in
    let rec loop () =
      match peek () with
      | None -> ()
      | Some c when is_whitespace c -> ()
      | Some ('(' | ')' | '"') -> ()
      | Some (_ : char) ->
        incr pos;
        loop ()
    in
    loop ();
    String.sub string ~pos:start ~len:(!pos - start)
  in
  let rec parse_form () : Sexp.t =
    skip_whitespace ();
    match peek () with
    | None -> parse_error "unexpected end of input"
    | Some '(' ->
      incr pos;
      parse_list Reversed_list.[]
    | Some ')' -> parse_error "unexpected close paren"
    | Some '"' ->
      incr pos;
      Atom (parse_quoted_string ())
    | Some (_ : char) ->
      (match parse_bare_atom () with
       (* In Elisp, [nil] is the empty list; [prin1] just renders it as [nil] rather than
          [()]. Mapping it back to [List []] makes [of_value] invert [to_form_elisp]:
          OCaml atoms always convert to Elisp strings, which print quoted, so a bare [nil]
          can only have come from an empty list. *)
       | "nil" -> List []
       | (_ : string) -> parse_error "unsupported bare atom")
  and parse_list (rev_forms : Sexp.t Reversed_list.t) =
    skip_whitespace ();
    match peek () with
    | None -> parse_error "unexpected end of input"
    | Some ')' ->
      incr pos;
      List (Reversed_list.rev rev_forms)
    | Some (_ : char) ->
      let form = parse_form () in
      parse_list (form :: rev_forms)
  in
  let form = parse_form () in
  skip_whitespace ();
  if Int.( < ) !pos len then parse_error "trailing data";
  form
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
          |> of_prin1_output)
        (fun sexp -> to_form_elisp sexp |> Form.to_value)
    ;;
  end)

module Private = struct
  let of_prin1_output = of_prin1_output
end

let sexp_pretty_config =
  { (Sexp_pretty.Config.create ~color:false ()) with
    atom_printing =
      Minimal_escaping
      (* Allow verbatim newlines in sexp output. Elisp's sexp reader can handle this, and
         it makes large sexps easier to read. *)
  ; encoding =
      Utf8
      (* Print UTF-8 atoms verbatim rather than as [\ddd] escapes. Atoms that aren't valid
         UTF-8 still print escaped. *)
  }
;;

let () =
  Defun.defun
    ("ecaml-sexp-pp-to-string" |> Symbol.intern)
    [%here]
    ~docstring:"Pretty-print SEXP and return the result as a string."
    (Returns Value.Type.string)
    (let%map_open.Defun sexp = required "sexp" t in
     Sexp_pretty.pretty_string sexp_pretty_config sexp)
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
