open! Core_kernel
open! Import

(** A typeful interface for calling Elisp, as [external] does for C. E.g., {[

      module F = struct

        let not : bool -> bool = Q.not <: bool @-> return bool

      end

      let non f = Fn.compose F.not f

    ]} The [F] convention is borrowed from the Emacs C source.
*)

type 'a t

val return : 'a Value.Type.t -> 'a t

val return_nil : unit t

(** [Q.foo <: ...] types an Elisp function, like how [foo :> t] types an OCaml value. *)
val (<:) : Symbol.t -> 'a t -> 'a

val (@->) : 'a Value.Type.t -> 'b t -> ('a -> 'b) t

(** {1} Special cases for nullary functions *)

val nullary     : 'a Value.Type.t -> Symbol.t -> unit -> 'a
val nullary_nil :                    Symbol.t -> unit -> unit
