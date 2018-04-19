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
open! Import

type t = Process0.t [@@deriving sexp_of]

include Equal.S       with type t := t
include Value.Subtype with type t := t

(** Accessors *)
val buffer        : t -> Buffer.t option    (** [(describe-function 'process-buffer)] *)
val command       : t -> string list option (** [(describe-function 'process-command)] *)
val name          : t -> string             (** [(describe-function 'process-name)] *)
val pid           : t -> Pid.t option       (** [(describe-function 'process-id)] *)
val query_on_exit : t -> bool               (** [(describe-function 'process-query-on-exit-flag)] *)
val status        : t -> Symbol.t           (** [(describe-function 'process-status)] *)

(** [set_query_on_exit t] specifies whether Emacs should query the user about killing [t]
    when it exits.  [(describe-function 'set-process-query-on-exit-flag)]. *)
val set_query_on_exit : t -> bool -> unit

(** [(describe-function 'process-list)] *)
val all_emacs_children : unit -> t list

(** [(Info-goto-node "(elisp)Asynchronous Processes")]
    [(describe-function 'start-process)] *)
val create
  :  ?buffer : Buffer.t
  -> unit
  -> args : string list
  -> name : string
  -> prog : string
  -> t

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
      | Split of { stderr : Stderr.t
                 ; stdout : Stdout.t }
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
  :  ?input               : Call.Input.t         (** default is [Dev_null] *)
  -> ?output              : Call.Output.t        (** default is [Dev_null] *)
  -> ?redisplay_on_output : bool                 (** default is [false] *)
  -> ?working_directory   : Working_directory.t  (** default is [Root] *)
  -> string
  -> string list
  -> Call.Result.t

(** [call_exn] runs [call_result_exn], strips whitespace from stdout+stderr if
    [strip_whitespace] is [true], and returns the resulting string, raising on
    nonzero exit. *)
val call_exn
  :  ?input             : Call.Input.t         (** default is [Dev_null] *)
  -> ?working_directory : Working_directory.t  (** default is [Root] *)
  -> ?strip_whitespace  : bool                 (** default is [true] *)
  -> string
  -> string list
  -> string

(** [call_expect_no_output_exn] runs [call_result_exn] and raises if the command output is
    not the empty string or on nonzero exit. *)
val call_expect_no_output_exn
  :  ?input             : Call.Input.t         (** default is [Dev_null] *)
  -> ?working_directory : Working_directory.t  (** default is [Root] *)
  -> ?strip_whitespace  : bool                 (** default is [false] *)
  -> string
  -> string list
  -> unit

val shell_command_result
  :  ?input               : Call.Input.t         (** default is [Dev_null] *)
  -> ?output              : Call.Output.t        (** default is [Dev_null] *)
  -> ?redisplay_on_output : bool                 (** default is [false] *)
  -> ?working_directory   : Working_directory.t  (** default is [Root] *)
  -> string
  -> Call.Result.t

(** [shell_command_exn command] runs [command] in a subshell, strips whitespace from
    stdout+stderr, and returns the resulting string, raising on nonzero exit. *)
val shell_command_exn
  :  ?input             : Call.Input.t         (** default is [Dev_null] *)
  -> ?working_directory : Working_directory.t  (** default is [Root] *)
  -> string
  -> string

val shell_command_expect_no_output_exn
  :  ?input             : Call.Input.t         (** default is [Dev_null] *)
  -> ?working_directory : Working_directory.t  (** default is [Root] *)
  -> string
  -> unit

(** [(Info-goto-node "(elisp)Network Servers")]
    [(describe-function 'make-network-process)] *)
val create_unix_network_process
  :  unit
  -> filter      : (t -> Text.t -> unit)
  -> name        : string
  -> socket_path : string
  -> t

(** [(Info-goto-node "(elisp)Deleting Processes")], [(describe-function
    'delete-process)]. *)
val kill : t -> unit
