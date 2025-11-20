open! Core
open! Import
include Buffer_local_intf
module Current_buffer = Current_buffer0

module Q = struct
  let permanent_local = "permanent-local" |> Symbol.intern
end

type 'a t = 'a Var.t [@@deriving sexp_of]

let symbol = Var.symbol
let var (t : _ t) : _ Var.t = t

let wrap_existing ?(make_buffer_local_always = false) symbol type_ =
  (* We raise if [symbol] isn't bound. This fits with the naming [wrap_existing symbol]
     implying that [symbol] exists. Also, it avoids calling [Var.make_buffer_local_always]
     on an unbound variable, which would set the variable to nil, which is probably not
     desired. *)
  if not (Current_buffer.variable_is_defined symbol)
  then
    raise_s
      [%message "[Buffer_local.wrap_existing] of undefined symbol" (symbol : Symbol.t)];
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

module Wrap = struct
  let ( <: ) ?make_buffer_local_always name type_ =
    wrap_existing ?make_buffer_local_always (name |> Symbol.intern) type_
  ;;

  include (Value.Type : Value.Type.S)
end

let defvar ~(here : [%call_pos]) symbol ?docstring ~type_ ~default_value () =
  Defvar.defvar_local
    symbol
    here
    ~docstring:(Option.value docstring ~default:"An Ecaml buffer-local.")
    ~type_
    ~default_value
    ()
;;

let defvar_embedded
  (type a)
  ~(here : [%call_pos])
  ?docstring
  symbol
  (module Arg : Defvar_embedded_arg with type t = a)
  =
  defvar
    symbol
    ~here
    ?docstring
    ~type_:
      (Value.Type.nil_or
         (Caml_embed.create_type
            (Type_equal.Id.create ~name:(Symbol.name symbol) [%sexp_of: Arg.t])))
    ~default_value:None
    ()
;;

let set_in_current_buffer t a = Current_buffer.set_value t a

let set t a buffer =
  Current_buffer.set_temporarily Sync buffer ~f:(fun () -> set_in_current_buffer t a)
;;

let set_temporarily_in_current_buffer sync_or_async t a ~f =
  Current_buffer.set_value_temporarily sync_or_async t a ~f
;;

let[@cold] raise_buffer_has_strange_value (variable : _ Var.t) buffer =
  raise_s
    [%message
      "buffer has strange value for variable"
        (variable : _ Var.t)
        (buffer : Buffer.t)
        ~value:(Current_buffer.symbol_value variable.symbol : Value.t)]
;;

let get_in_current_buffer t =
  match Current_buffer.value_exn t with
  | t -> t
  | exception _ -> raise_buffer_has_strange_value t (Current_buffer.get ())
;;

let buffer_local_value =
  Funcall.Wrap.("buffer-local-value" <: Symbol.t @-> Buffer.t @-> return value)
;;

let get t buffer =
  let value = buffer_local_value (symbol t) buffer in
  try Value.Type.of_value_exn (var t).type_ value with
  | _ -> raise_buffer_has_strange_value t buffer
;;

let get_var = get

let raise_buffer_has_no_value_for_variable t ~buffer =
  raise_s
    [%message
      "buffer has no value for variable" ~variable:(t : _ Var.t) (buffer : Buffer.t)]
;;

let get_in_current_buffer_exn t =
  match get_in_current_buffer t with
  | Some x -> x
  | None -> raise_buffer_has_no_value_for_variable t ~buffer:(Current_buffer.get ())
;;

let get_exn t buffer =
  match get t buffer with
  | Some x -> x
  | None -> raise_buffer_has_no_value_for_variable t ~buffer
;;

let update_exn t buffer ~f =
  Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
    set_in_current_buffer t (Some (f (get_in_current_buffer_exn t))))
;;

let permanent_property = Symbol.Property.create Q.permanent_local Value.Type.bool

let set_permanent t permanent =
  Symbol.Property.put permanent_property (symbol t) permanent
;;

let is_permanent t =
  Symbol.Property.get permanent_property (symbol t) |> Option.value ~default:false
;;

module Private = struct
  let get_in_current_buffer = get_in_current_buffer
  let get_in_current_buffer_exn = get_in_current_buffer_exn
  let set_in_current_buffer = set_in_current_buffer
  let set_temporarily_in_current_buffer = set_temporarily_in_current_buffer
end
