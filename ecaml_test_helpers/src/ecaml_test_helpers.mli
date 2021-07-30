open! Core
open! Async
module Buffer_helper : module type of Buffer_helper

(** Run the given key sequence, and then display the current buffer. *)
val press
  :  ?and_show:bool (** default: [true] *)
  -> ?show_point:bool (** default: [true] *)
  -> string
  -> unit Deferred.t

(** Run the given key sequence, and then show the following:

    - minibuffer prompt
    - minibuffer contents, unless [show_contents = false]
    - *Completions* buffer, if any
*)
val press_and_show_minibuffer
  :  ?show_contents:bool (** default: [true] *)
  -> string
  -> unit Deferred.t

(** Display the current buffer. *)
val show : ?show_point:bool (** default: [true] *) -> unit -> unit

(** Evaluate an elisp expression and display the resulting value. *)
val eval : string -> unit Deferred.t
