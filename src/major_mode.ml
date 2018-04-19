open! Core_kernel
open! Import

module Q = struct
  include Q
  let define_derived_mode              = "define-derived-mode"              |> Symbol.intern
  let fundamental_mode                 = "fundamental-mode"                 |> Symbol.intern
  let prog_mode                        = "prog-mode"                        |> Symbol.intern
  let special_mode                     = "special-mode"                     |> Symbol.intern
  let text_mode                        = "text-mode"                        |> Symbol.intern
end

module Current_buffer = Current_buffer0

type t =
  { change_command   : Symbol.t
  ; keymap_var       : Keymap.t Var.t
  ; syntax_table_var : Syntax_table.t Var.t }
[@@deriving fields, sexp_of]

let equal t1 t2 = Symbol.equal t1.change_command t2.change_command

let create ~change_command =
  { change_command
  ; keymap_var =
      Var.create
        (Symbol.intern (concat [ change_command |> Symbol.name ; "-map" ]))
        Keymap.type_
  ; syntax_table_var =
      Var.create
        (Symbol.intern (concat [ change_command |> Symbol.name ; "-syntax-table" ]))
        Syntax_table.type_ }
;;

let keymap       t = Current_buffer.value_exn t.keymap_var
let keymap_var   t = t.keymap_var
let syntax_table t = Current_buffer.value_exn t.syntax_table_var

let fundamental = create ~change_command:Q.fundamental_mode
let prog        = create ~change_command:Q.prog_mode
let special     = create ~change_command:Q.special_mode
let text        = create ~change_command:Q.text_mode

let define_derived_mode
      ?parent
      here
      ~change_command
      ~docstring
      ~initialize
      ~mode_line
  =
  Form.eval_i (
    Form.list
      [ Q.define_derived_mode |> Form.symbol
      ; change_command |> Form.symbol
      ; (match parent with
         | None -> Form.nil
         | Some t -> Field.get Fields.change_command t |> Form.symbol)
      ; mode_line |> Form.string
      ; docstring |> Form.string
      ; Form.list
          [ Q.funcall |> Form.symbol
          ; Form.quote (
              Function.create here ~args:[]
                (fun _ -> initialize (); Value.nil)
              |> Function.to_value) ]]);
  Load_history.add_entry here (Fun change_command);
  List.iter
    [ "abbrev-table"
    ; "hook"
    ; "map"
    ; "syntax-table" ]
    ~f:(fun suffix ->
      Load_history.add_entry here (
        Var (concat [ change_command |> Symbol.name; "-"; suffix ] |> Symbol.intern)));
  create ~change_command
;;
