(** [Process] is used to create "subprocesses" or "child processes" of the Emacs process,
    which is their "parent process".  A subprocess of Emacs may be "synchronous" or
    "asynchronous", depending on how it is created.  When you create a synchronous
    subprocess, the program waits for the subprocess to terminate before continuing
    execution.  When you create an asynchronous subprocess, it can run in parallel with
    Emacs.  This kind of subprocess is represented within Emacs by a [Process.t].
    Programs can use this object to communicate with the subprocess or to control it.  For
    example, you can send signals, obtain status information, receive output from the
    process, or send input to it.

    [(Info-goto-node "(elisp)Processes")]. *)

open! Core_kernel
open! Async_kernel
open! Import

type t = Process0.t [@@deriving sexp_of]

include Equal.S with type t := t
include Value.Subtype with type t := t

(** Accessors *)

(** [(describe-function 'process-buffer)] *)
val buffer : t -> Buffer.t option

(** [(describe-function 'process-command)] *)
val command : t -> string list option

(** [(describe-function 'process-name)] *)
val name : t -> string

(** [(describe-function 'process-id)] *)
val pid : t -> Pid.t option

(** [(describe-function 'process-query-on-exit-flag)] *)
val query_on_exit : t -> bool

module Status : sig
  type t =
    | Closed
    | Connect
    | Exit
    | Failed
    | Listen
    | Open
    | Run
    | Signal
    | Stop
  [@@deriving sexp_of]

  include Valueable.S with type t := t
end

(** [(describe-function 'process-status)] *)
val status : t -> Status.t

module Exit_status : sig
  type t =
    | Not_exited
    | Exited of int
    | Fatal_signal of int
  [@@deriving sexp_of]
end

(** [(describe-function 'process-exit-status)] *)
val exit_status : t -> Exit_status.t

module Exited : sig
  type t =
    | Exited of int
    | Fatal_signal of int
  [@@deriving sexp_of]

  val successfully : t -> bool
end

val exited : t -> Exited.t Deferred.t

(** [(describe-function 'process-live-p)] *)
val is_alive : t -> bool

(** [(describe-function 'process-mark)] *)
val mark : t -> Marker.t

(** [set_query_on_exit t] specifies whether Emacs should query the user about killing [t]
    when it exits.  [(describe-function 'set-process-query-on-exit-flag)]. *)
val set_query_on_exit : t -> bool -> unit

(** [(describe-function 'process-list)] *)
val all_emacs_children : unit -> t list

(** [(describe-function 'process-get)] *)
val get_property : t -> Symbol.t -> Value.t option

(** [(describe-function 'process-put)] *)
val set_property : t -> Symbol.t -> Value.t -> unit

(** [(Info-goto-node "(elisp)Asynchronous Processes")]
    [(describe-function 'start-process)] *)
val create : string -> string list -> name:string -> ?buffer:Buffer.t -> unit -> t

(** [(describe-function 'get-process)]
    [(Info-goto-node "(elisp)Process Information")] *)
val find_by_name : string -> t option

module Call : sig
  module Input : sig
    type t =
      | Dev_null
      | File of string
    [@@deriving sexp_of]
  end

  module Region_input : sig
    type t =
      | Region of
          { start : Position.t
          ; end_ : Position.t
          ; delete : bool
          }
      | String of string
    [@@deriving sexp_of]
  end

  module Output : sig
    module Stdout : sig
      type t =
        | Before_point_in of Buffer.t
        | Before_point_in_current_buffer
        | Dev_null
        | Overwrite_file of string
      [@@deriving sexp_of]
    end

    module Stderr : sig
      type t =
        | Dev_null
        | Overwrite_file of string
      [@@deriving sexp_of]
    end

    type t =
      | Before_point_in of Buffer.t
      | Before_point_in_current_buffer
      | Dev_null
      | Overwrite_file of string
      | Split of
          { stderr : Stderr.t
          ; stdout : Stdout.t
          }
    [@@deriving sexp_of]
  end

  module Result : sig
    type t =
      | Exit_status of int
      | Signaled of string
    [@@deriving sexp_of]
  end
end

(** [(Info-goto-node "(elisp)Synchronous Processes")]
    [(describe-function 'call-process)] *)
val call_result_exn
  :  ?input:Call.Input.t (** default is [Dev_null] *)
  -> ?output:Call.Output.t (** default is [Dev_null] *)
  -> ?redisplay_on_output:bool (** default is [false] *)
  -> ?working_directory:Working_directory.t (** default is [Root] *)
  -> string
  -> string list
  -> Call.Result.t

(** [(Info-goto-node "(elisp)Synchronous Processes")]
    [(describe-function 'call-process-region)] *)
val call_region_exn
  :  ?input:Call.Region_input.t
  (** default is [Region { start = Point.min (); end_ = Point.max ()}] *)
  -> ?output:Call.Output.t (** default is [Dev_null] *)
  -> ?redisplay_on_output:bool (** default is [false] *)
  -> ?working_directory:Working_directory.t (** default is [Root] *)
  -> string
  -> string list
  -> Call.Result.t

(** [call_exn] runs [call_result_exn], strips whitespace from stdout+stderr if
    [strip_whitespace] is [true], and returns the resulting string, raising on
    nonzero exit. *)
val call_exn
  :  ?input:Call.Input.t (** default is [Dev_null] *)
  -> ?working_directory:Working_directory.t (** default is [Root] *)
  -> ?strip_whitespace:bool (** default is [true] *)
  -> ?verbose_exn:bool (** default is [true] *)
  -> string
  -> string list
  -> string

(** [call_expect_no_output_exn] runs [call_result_exn] and raises if the command output is
    not the empty string or on nonzero exit. *)
val call_expect_no_output_exn
  :  ?input:Call.Input.t (** default is [Dev_null] *)
  -> ?working_directory:Working_directory.t (** default is [Root] *)
  -> ?strip_whitespace:bool (** default is [false] *)
  -> ?verbose_exn:bool (** default is [true] *)
  -> string
  -> string list
  -> unit

val shell_command_result
  :  ?input:Call.Input.t (** default is [Dev_null] *)
  -> ?output:Call.Output.t (** default is [Dev_null] *)
  -> ?redisplay_on_output:bool (** default is [false] *)
  -> ?working_directory:Working_directory.t (** default is [Root] *)
  -> string
  -> Call.Result.t

(** [shell_command_exn command] runs [command] in a subshell, strips whitespace from
    stdout+stderr, and returns the resulting string, raising on nonzero exit. *)
val shell_command_exn
  :  ?input:Call.Input.t (** default is [Dev_null] *)
  -> ?working_directory:Working_directory.t (** default is [Root] *)
  -> ?verbose_exn:bool (** default is [true] *)
  -> string
  -> string

val shell_command_expect_no_output_exn
  :  ?input:Call.Input.t (** default is [Dev_null] *)
  -> ?working_directory:Working_directory.t (** default is [Root] *)
  -> ?verbose_exn:bool (** default is [true] *)
  -> string
  -> unit

(** [(Info-goto-node "(elisp)Network Servers")]
    [(describe-function 'make-network-process)]

    The [t] returned by [create_unix_network_process] represents listening on the socket.
    The [t] passed to [filter] represents a specific connection accepted on the socket. *)
val create_unix_network_process
  :  unit
  -> filter:(t -> Text.t -> unit)
  -> name:string
  -> socket_path:string
  -> t

(** [(Info-goto-node "(elisp)Deleting Processes")]
    [(describe-function 'delete-process)]. *)
val kill : t -> unit

(** [(Info-goto-node "(elisp)Sentinels")]

    Register [sentinel] as a process sentinel for the specified process.
    [sentinel] runs after any other processes sentinels set for that process. *)
val extend_sentinel
  :  Source_code_position.t
  -> t
  -> (unit, 'a) Defun.Returns.t
  -> sentinel:(event:string -> 'a)
  -> unit

(** Prefer [extend_sentinel], which doesn't clobber existing sentinels.
    [(describe-function 'set-process-sentinel)] *)
val set_sentinel
  :  Source_code_position.t
  -> t
  -> (unit, 'a) Defun.Returns.t
  -> sentinel:(event:string -> 'a)
  -> unit
