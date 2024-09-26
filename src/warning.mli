(** [(Info-goto-node "(elisp) Warnings")] *)

open! Core
open! Async
open! Import

module Level : sig
  type t =
    | Emergency
    | Error
    | Warning
    | Debug
end

val display : string -> type_:Symbol.t Nonempty_list.t -> level:Level.t -> unit
val display_text : Text.t -> type_:Symbol.t Nonempty_list.t -> level:Level.t -> unit
