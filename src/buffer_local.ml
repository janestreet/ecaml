open! Core
open! Import
include Buffer_local_intf
module Current_buffer = Current_buffer0

type 'a t = 'a Var.t [@@deriving sexp_of]

let symbol = Var.symbol
let var (t : _ t) : _ Var.t = t

let wrap_existing ?(make_buffer_local_always = false) symbol type_ =
  let var = Var.create symbol type_ in
  if make_buffer_local_always
  then Var.make_buffer_local_always var
  else if not (Var.is_buffer_local_always var)
  then
    raise_s
      [%message
        {|[Buffer_local.wrap_existing] on an Elisp variable that is not automatically buffer local|}
          (symbol : Symbol.t)];
  var
;;

let defvar symbol here ?(docstring = "") ~type_ ~default_value () =
  let var =
    Defvar.defvar symbol here ~docstring ~type_ ~initial_value:default_value ()
  in
  Var.make_buffer_local_always var;
  var
;;

let defvar_embedded
      (type a)
      symbol
      here
      ?docstring
      (module Arg : Defvar_embedded_arg with type t = a)
  =
  defvar
    symbol
    here
    ?docstring
    ~type_:
      (Value.Type.option
         (Value.Type.caml_embed
            (Type_equal.Id.create ~name:(Symbol.name symbol) [%sexp_of: Arg.t])))
    ~default_value:None
    ()
;;

let set_in_current_buffer t a = Current_buffer.set_value t a

let set t a buffer =
  Current_buffer.set_temporarily buffer ~f:(fun () -> set_in_current_buffer t a)
;;

let get_in_current_buffer t =
  match Current_buffer.value_exn t with
  | t -> t
  | exception _ ->
    raise_s
      [%message
        "buffer has strange value for variable"
          ~variable:(t : _ Var.t)
          ~buffer:(Current_buffer.get () : Buffer.t)
          ~value:(Current_buffer.symbol_value t.symbol : Value.t)]
;;

let get t buffer =
  Current_buffer.set_temporarily buffer ~f:(fun () -> get_in_current_buffer t)
;;

let get_in_current_buffer_exn t =
  match get_in_current_buffer t with
  | Some x -> x
  | None ->
    raise_s
      [%message
        "buffer has no value for variable"
          ~variable:(t : _ Var.t)
          ~buffer:(Current_buffer.get () : Buffer.t)]
;;

let get_exn t buffer =
  Current_buffer.set_temporarily buffer ~f:(fun () -> get_in_current_buffer_exn t)
;;

module Private = struct
  let get_in_current_buffer = get_in_current_buffer
  let get_in_current_buffer_exn = get_in_current_buffer_exn
  let set_in_current_buffer = set_in_current_buffer
end
