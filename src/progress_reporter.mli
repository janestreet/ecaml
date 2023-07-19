(** Report progress of long-running operations in the echo area.

    [(Info-goto-node "(elisp) Progress")] *)

open! Core
open! Async_kernel
open! Import

(** {1 Primitives} *)

type 'a t [@@deriving sexp_of]

(** Create a progress reporter with no predefined min and max values.

    Updating the reporter causes a spinner to be animated in the echo area. *)
val create_spinner_and_display : message:string -> unit t

(** Create a progress reporter with a predefined min and max value, representing the
    starting and final states of the operation.

    For example, when scanning a buffer, one might use:

    {[
      create_with_range_and_display
        ~min_incl:(Point.min ())
        ~max_incl:(Point.max ())
        ~message:"..."
    ]}

    The initial state of the progress reporter is [min_incl].

    Updating the reporter causes a percentage to be displayed in the echo area. *)
val create_with_range_and_display
  :  min_incl:int
  -> max_incl:int
  -> message:string
  -> int t

(** Update the progress reporter with the current state of the operation.

    [suffix], if provided, is displayed after the main message and progress indicator. *)
val update : ?suffix:string -> 'a t -> 'a -> unit

(** Report that the operation is finished. *)
val finish : _ t -> unit

(** {2 Convenience functions} *)

module Deferred : sig
  module List : sig
    (** [suffix elt], if provided, is called after [f elt] is determined, and the string
        it returns is displayed after the progress indicator. *)
    val iter
      :  ?suffix:('a -> string)
      -> message:string
      -> 'a list
      -> how:Monad_sequence.how
      -> f:('a -> unit Deferred.t)
      -> unit Deferred.t
  end
end
