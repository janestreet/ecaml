open! Core_kernel
open! Import

(** A typeful interface for calling Elisp, as [external] does for C. E.g., {[

      module F = struct

        let not : bool -> bool = Q.not <: bool @-> return bool

        let about_emacs : unit -> unit = Q.about_emacs <: nullary @-> return nil

      end

      let non f = Fn.compose F.not f

      let about_emacs = F.about_emacs

    ]} The [F] convention is borrowed from the Emacs C source.
*)

type 'a t

val return : 'a Value.Type.t -> 'a t

val nil     : unit Value.Type.t
val nullary : unit Value.Type.t

(** [Q.foo <: ...] types an Elisp function, like how [foo :> t] types an OCaml value. *)
val (<:) : Symbol.t -> 'a t -> 'a

val (@->) : 'a Value.Type.t -> 'b t -> ('a -> 'b) t
