open! Core_kernel
open! Import0
include Value.Subtype
include Equal.S with type t := t

type window := t

module Edges : sig
  (** [(Info-goto-node "(elisp) Coordinates and Windows")] *)
  type t =
    { bottom : int
    ; left : int
    ; right : int
    ; top : int
    }
  [@@deriving sexp_of]

  include Valueable.S with type t := t
end

module Tree : sig
  (** [(Info-goto-node "(elisp) Windows and Frames")] *)

  module Direction : sig
    type t =
      | Left_to_right (** "horizontal" in emacs parlance *)
      | Top_to_bottom (** "vertical" in emacs parlance *)
    [@@deriving sexp_of]

    include Valueable.S with type t := t
  end

  type t =
    | Combination of
        { children : t list
        ; direction : Direction.t
        ; edges : Edges.t
        }
    | Window of window
  [@@deriving sexp_of]

  include Valueable.S with type t := t

  (** [parent_exn t window] returns the [Combination] node that is the parent of [Window
      window] in [t]. If [t = Window window], it returns [t]. Raises if [window] is not in
      [t]. *)
  val parent_exn : t -> window -> t
end
