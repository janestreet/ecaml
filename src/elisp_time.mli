(** Elisp's analog of [Core.Time_ns], but with picosecond precision and a wider range of
    allowed times.

    [(Info-goto-node "(elisp)Time of Day")] *)

open! Core_kernel
open! Import

type t = private Value.t [@@deriving compare]

include Value.Subtype with type t := t

(** [(describe-function 'format-time-string)]
    [(Info-goto-node "(elisp)Time Parsing")] *)
val format
  :  ?zone:Time.Zone.t (** default is Emacs local time *)
  -> t
  -> format_string:string
  -> string

val of_time_ns : Time_ns.t -> t

(** [to_time_ns_exn t] ignores the sub-nanonsecond component of [t].  It raises if [t] is
    outside [Time_ns.min_value] and [Time_ns.max_value]. *)
val to_time_ns_exn : t -> Time_ns.t
