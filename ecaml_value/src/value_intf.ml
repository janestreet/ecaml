(** [Value.t] is the OCaml type corresponding to Emacs's universal type of values.  It is
    represented as an OCaml custom block ([emacs_value_ops] in [ecaml_stubs.c]) wrapped
    around the [emacs_value] pointer type defined by the Emacs native-code module
    interface, [emacs-module.h], available in Emacs 25 and beyond.  This module has
    low-level functions for working with Emacs values, OCaml wrappers that call the C
    functions specified in [emacs-module.h].  All other calls from OCaml to Emacs are
    built on top of this module. *)

open! Core_kernel
open! Import
open! Async_kernel

module type Make_subtype_arg = sig
  type value

  val here : Source_code_position.t
  val name : string
  val is_in_subtype : value -> bool
end

type 'a funcall = ?should_profile:bool -> 'a

module type Funcall = sig
  type t
  type value

  val funcall0 : (t -> value) funcall
  val funcall1 : (t -> value -> value) funcall
  val funcall2 : (t -> value -> value -> value) funcall
  val funcall3 : (t -> value -> value -> value -> value) funcall
  val funcall4 : (t -> value -> value -> value -> value -> value) funcall
  val funcall5 : (t -> value -> value -> value -> value -> value -> value) funcall
  val funcallN : (t -> value list -> value) funcall
  val funcallN_array : (t -> value array -> value) funcall
  val funcall0_i : (t -> unit) funcall
  val funcall1_i : (t -> value -> unit) funcall
  val funcall2_i : (t -> value -> value -> unit) funcall
  val funcall3_i : (t -> value -> value -> value -> unit) funcall
  val funcall4_i : (t -> value -> value -> value -> value -> unit) funcall
  val funcall5_i : (t -> value -> value -> value -> value -> value -> unit) funcall
  val funcallN_i : (t -> value list -> unit) funcall
  val funcallN_array_i : (t -> value array -> unit) funcall

  val funcall_int_int_value_value_unit
    : (t -> int -> int -> value -> value -> unit) funcall

  val funcall_int_int_value_unit : (t -> int -> int -> value -> unit) funcall
end

module type Subtype = sig
  type value

  (** We expose [private value] for free identity conversions when the value is nested in
      some covariant type, e.g. [(symbols : Symbol.t list :> Value.t list)] rather than
      [List.map symbols ~f:Symbol.to_value]. *)
  type t = private value [@@deriving sexp_of]

  (** [eq t1 t2 = Value.eq (to_value t1) (to_value t2)], i.e. [eq] checks whether the
      Emacs values underlying [t1] and [t2] are physically equal.  This is different than
      [phys_equal t1 t2], because we don't always wrap [eq] Emacs values in [phys_equal]
      OCaml values.  I.e. [phys_equal t1 t2] implies [eq t1 t2], but not the converse. *)
  val eq : t -> t -> bool

  val is_in_subtype : value -> bool

  include Valueable0.S with type t := t
end

module type Type = sig
  type value
  type 'a t

  val create : Sexp.t -> ('a -> Sexp.t) -> (value -> 'a) -> ('a -> value) -> 'a t
  val with_of_value_exn : 'a t -> (value -> 'a) -> 'a t
  val to_sexp : 'a t -> 'a -> Sexp.t
  val bool : bool t
  val float : float t
  val ignored : unit t
  val int : int t
  val string : string t

  (** [string_cached] is like [string], except it uses [of_utf8_bytes_cached]. *)
  val string_cached : string t

  val unit : unit t
  val value : value t
  val list : 'a t -> 'a list t
  val vector : 'a t -> 'a array t

  (** Represent an ocaml array as an elisp list, without creating an intermediate ocaml
      list. *)
  val array_as_list : 'a t -> 'a array t

  (** [option] represents [None] as [nil] and [Some a] as [cons v nil], where [v] is the
      representation of [a]. *)
  val option : 'a t -> 'a option t

  (** [nil_or t_] represents [None] as [nil] and [Some a] as [v], where [v] is the
      representation of [a].  This is a common representation used by Elisp functions.
      But it is only correct if [nil] is not a representation of any value in [t]; in that
      situation use [Type.option_]. *)
  val nil_or : 'a t -> 'a option t

  val alist : 'a t -> 'b t -> ('a * 'b) list t

  (** Represent a tuple (a,b) as the elisp cons cell (a . b) *)
  val tuple : 'a t -> 'b t -> ('a * 'b) t

  (** Represent a tuple (a,b) as the elisp list '(a b) *)
  val tuple2_as_list : 'a t -> 'b t -> ('a * 'b) t

  (** Embed a sexpable ocaml type, so we can save values of the type in emacs, e.g. as
      buffer local variables *)
  val sexpable : (module Sexpable with type t = 'a) -> name:Sexp.t -> 'a t

  (** A list of directories. Each element is a string (directory name) or nil (try
      default directory). nil values are converted to ".", which has the same meaning. *)
  val path_list : string list t
end

module type Enum = sig
  type t [@@deriving enumerate, sexp_of]
end

module type Value = sig
  type t = Value0.t [@@deriving sexp_of]

  include Funcall with type t := t with type value := t

  val intern : string -> t
  val nil : t
  val t : t
  val list : t list -> t

  (** [(describe-function 'cons)] *)
  val cons : t -> t -> t

  (** [(describe-function 'car)] *)
  val car_exn : t -> t

  (** [(describe-function 'cdr)] *)
  val cdr_exn : t -> t

  val to_list_exn : t -> f:(t -> 'a) -> 'a list
  val vector : t array -> t
  val to_array_exn : t -> f:(t -> 'a) -> 'a array
  val type_of : t -> t

  (** Convert an elisp list to an ocaml array *)
  val list_to_array_exn : t -> f:(t -> 'a) -> 'a array

  (** - [(Info-goto-node "(elisp)Type Predicates")] *)

  (** [(describe-function 'arrayp)] *)
  val is_array : t -> bool

  (** [(describe-function 'bufferp)] *)
  val is_buffer : t -> bool

  (** [(describe-function 'commandp)] *)
  val is_command : t -> bool

  (** [(describe-function 'eventp)] *)
  val is_event : t -> bool

  (** [(describe-function 'floatp)] *)
  val is_float : t -> bool

  (** [(describe-function 'fontp)] *)
  val is_font : t -> bool

  (** [(describe-function 'framep)] *)
  val is_frame : t -> bool

  (** [(describe-function 'functionp)] *)
  val is_function : t -> bool

  (** [(describe-function 'hash-table-p)] *)
  val is_hash_table : t -> bool

  (** [(describe-function 'integerp)] *)
  val is_integer : t -> bool

  (** [(describe-function 'keymapp)] *)
  val is_keymap : t -> bool

  (** [(describe-function 'markerp)] *)
  val is_marker : t -> bool

  (** [eq t nil] *)
  val is_nil : t -> bool

  (** [(describe-function 'is-not-nil)] *)
  val is_not_nil : t -> bool

  (** [(describe-function 'processp)] *)
  val is_process : t -> bool

  (** [(describe-function 'stringp)] *)
  val is_string : t -> bool

  (** [(describe-function 'symbolp)] *)
  val is_symbol : t -> bool

  (** [(describe-function 'syntax-table-p)] *)
  val is_syntax_table : t -> bool

  (** [(describe-function 'timerp)] *)
  val is_timer : t -> bool

  (** [(describe-function 'vectorp)] *)
  val is_vector : t -> bool

  (** [(describe-function 'windowp)] *)
  val is_window : t -> bool

  (** [(describe-function 'window-configuration-p)] *)
  val is_window_configuration : t -> bool

  (** [(describe-function 'consp)]

      If supplied, [?car] and [?cdr] are additionally required to return true for the car
      and cdr, respectively. *)
  val is_cons
    :  ?car:(t -> bool) (** default: const true *)
    -> ?cdr:(t -> bool) (** default: const true *)
    -> t
    -> bool

  val eq : t -> t -> bool

  (** [(describe-function 'equal)] *)
  val equal : t -> t -> bool

  val of_bool : bool -> t

  (** [is_not_nil] *)
  val to_bool : t -> bool

  (** [(describe-variable 'most-negative-fixnum)] *)
  val emacs_min_int : int

  (** [(describe-variable 'most-positive-fixnum)] *)
  val emacs_max_int : int

  (** [of_int_exn n] raises if [n] is not in the range [emacs_min_int, emacs_max_int] *)
  val of_int_exn : int -> t

  val to_int_exn : t -> int
  val of_float : float -> t
  val to_float_exn : t -> float
  val of_utf8_bytes : string -> t

  (** [of_utf8_bytes_cached] is like [of_utf8_bytes], except it keeps a hash table mapping
      each OCaml string to the corresponding Elisp string.  This can be used to optimize
      the conversion of OCaml values to Elisp values. *)
  val of_utf8_bytes_cached : string -> t

  val to_utf8_bytes_exn : t -> string
  val vec_get : t -> int -> t
  val vec_set : t -> int -> t -> unit
  val vec_size : t -> int
  val message : string -> unit
  val messagef : ('a, unit, string, unit) format4 -> 'a
  val message_s : Sexp.t -> unit
  val prin1_to_string : t -> string

  (** An ['a Type.t] is an isomorphism between ['a] and a subset of [Value.t]. *)
  module Type : sig
    type value
    type 'a t [@@deriving sexp_of]

    module type S = Type with type value := value with type 'a t := 'a t

    include S

    val id : 'a t -> 'a Type_equal.Id.t
    val to_value : 'a t -> 'a -> value
    val of_value_exn : 'a t -> value -> 'a
    val name : _ t -> Sexp.t
    val map : 'a t -> name:Sexp.t -> of_:('a -> 'b) -> to_:('b -> 'a) -> 'b t

    (** [map_id type_ name] is short for [map type_ ~name ~of_:Fn.id ~to_:Fn.id].
        It is not interchangeable with [type_] itself. *)
    val map_id : 'a t -> Sexp.t -> 'a t

    module type Enum = Enum

    val enum : Sexp.t -> (module Enum with type t = 'a) -> ('a -> value) -> 'a t
  end
  with type value := t

  module type Funcall = Funcall with type value := t
  module type Make_subtype_arg = Make_subtype_arg with type value := t
  module type Subtype = Subtype with type value := t with type 'a type_ := 'a Type.t

  module Make_subtype (Subtype : Make_subtype_arg) : Subtype

  module Expert : sig
    val have_active_env : unit -> bool
    val raise_if_emacs_signaled : unit -> unit
    val non_local_exit_signal : exn -> unit
  end

  module Stat : sig
    type t =
      { emacs_free_performed : int
      ; emacs_free_scheduled : int
      }
    [@@deriving sexp_of]

    val now : unit -> t
    val diff : t -> t -> t
  end

  module For_testing : sig
    val all_interned_symbols : unit -> string list

    exception
      Elisp_signal of
        { symbol : t
        ; data : t
        }

    exception
      Elisp_throw of
        { tag : t
        ; value : t
        }

    (** Used to edit non-deterministic stuff out of Elisp signals. *)
    val map_elisp_signal
      :  (unit -> 'a)
      -> f:(symbol:t -> data:t -> reraise:(symbol:t -> data:t -> Nothing.t) -> Nothing.t)
      -> 'a

    val map_elisp_signal_omit_data : (unit -> 'a) -> 'a
  end

  module Private : sig
    val ecaml_profile_print_length : int option ref
    val ecaml_profile_print_level : int option ref

    (** These functions are defined in Async_ecaml. This module exists to avoid
        dependency cycles. *)
    module Block_on_async : sig
      type t =
        { f :
            'a. Source_code_position.t -> ?context:Sexp.t Lazy.t
            -> (unit -> 'a Deferred.t) -> 'a
        }

      val set_once : t Set_once.t
    end

    module Enqueue_foreground_block_on_async : sig
      type t =
        { f :
            Source_code_position.t
            -> ?context:Sexp.t Lazy.t
            -> ?raise_exceptions_to_monitor:Monitor.t
            -> (unit -> unit Deferred.t)
            -> unit
        }

      val set_once : t Set_once.t
    end

    module Run_outside_async : sig
      type t =
        { f :
            'a. Source_code_position.t -> ?allowed_in_background:bool -> (unit -> 'a)
            -> 'a Deferred.t
        }

      val set_once : t Set_once.t
    end

    val block_on_async
      :  Source_code_position.t
      -> ?context:Sexp.t Lazy.t
      -> (unit -> 'a Deferred.t)
      -> 'a

    val enqueue_foreground_block_on_async
      :  Source_code_position.t
      -> ?context:Sexp.t Lazy.t
      -> ?raise_exceptions_to_monitor:Monitor.t
      -> (unit -> unit Deferred.t)
      -> unit

    val run_outside_async
      :  Source_code_position.t
      -> ?allowed_in_background:bool
      -> (unit -> 'a)
      -> 'a Deferred.t

    val message_zero_alloc : t -> unit

    (** [message_t t] is equivalent to the elisp expression [(message "%s" t)]. *)
    val message_t : t -> unit
  end
end
