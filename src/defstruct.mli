(** Wrapper for [cl-defstruct]. See [(Info-goto-node "(cl) Structures")].

    This module's interface is geared towards constructing structured Lisp values from
    closely-corresponding OCaml values. In particular, all fields of a need to decide
    ahead-of-time how to extract the Lisp value for a given slot from some OCaml value.

    For example,

    {[
      type person =
        { name : string
        ; age : int
        }

      let person_struct =
        defstruct
          ~name:"person"
          ~doc:"A person with a name and age."
          Field.
            [ field "name" (fun x -> x.name) string; field "age" (fun x -> x.age) int ]
      ;;
    ]}

    For now, this module only supports defining such types and constructing instances
    thereof. It does not yet support extracting values from structure slots. *)

open! Core
open! Import

(** Idiomatic usage of this module is to locally open it around a list of fields. See the
    module documentation of [Defstruct] for an example. *)
module Field : sig
  type 'a t

  (** [field name get type_] defines a field of the cl-defstruct.

      When constructing an instance of the structure type from an OCaml value [x], [get x]
      is called to extract the part of the OCaml value corresponding to this field.
      [type_] is then used to convert that value into an Elisp value. *)
  val field : string -> ('a -> 'b) -> 'b Value.Type.t -> 'a t

  include Value.Type.S
end

(** A Lisp structure type.

    The constructor for this structure type expects an ['a] as input. *)
type 'a t

(** Define a cl-defstruct with the specified fields.

    If [defined_by_feature] is passed, we won't actually call cl-defstruct; instead, when
    [Defstruct.make] is called we will automatically require that feature, which should
    define a compatible struct. If the field names don't match up exactly,
    [Defstruct.make] will error. *)
val defstruct
  :  here:[%call_pos]
  -> name:string
  -> ?defined_by_feature:Feature.t
  -> doc:string
  -> 'a Field.t list
  -> 'a t

(** Construct an instance of the structure type. *)
val make : 'a t -> 'a -> Value.t

module For_testing : sig
  val all_defstructs : unit -> (string * Symbol.t * Symbol.t list) list
end
