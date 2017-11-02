open! Core_kernel
open! Import
open! Syntax_table

let show t =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Current_buffer.set_syntax_table t;
    let by_class = Class.Table.create () in
    for i = 0 to 127 do
      let char = i |> Char.of_int_exn in
      if Char.is_print char
      then
        Hashtbl.add_multi by_class
          ~key:(Current_buffer.syntax_class (char |> Char_code.of_char_exn))
          ~data:char
    done;
    let by_class =
      Hashtbl.map by_class ~f:(fun chars ->
        String.of_char_list (List.rev chars)) in
    print_s [%sexp (by_class : string Class.Table.t)])
;;

let%expect_test "[standard]" =
  show standard;
  [%expect {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}];
;;

let%expect_test "[create]" =
  show (create ());
  [%expect {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}];
;;

let%expect_test "[create]" =
  show (copy (create ()));
  [%expect {|
    ((Close_paren         ")]}")
     (Escape              "\\")
     (Open_paren          "([{")
     (Punctuation         "!#',.:;?@^`~")
     (String_quote        "\"")
     (Symbol_constitutent &*+-/<=>_|)
     (Whitespace          " ")
     (Word_constituent
      $%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz)) |}];
;;

let%expect_test "[set_char]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let t = create () in
    Current_buffer.set_syntax_table t;
    let char = 'a' in
    List.iter Class.all ~f:(fun class_ ->
      match class_ with
      | Inherit_standard -> ()
      | _ ->
        set_char t char class_ [];
        let round_trip = Current_buffer.syntax_class (char |> Char_code.of_char_exn) in
        require [%here] (Class.equal class_ round_trip)
          ~if_false_then_print_s:(
            lazy [%message "" (class_ : Class.t) (round_trip : Class.t)])));
  [%expect {| |}];
;;
