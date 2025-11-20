open! Core
open! Async_kernel
open! Import

module Q = struct
  include Q

  let call_process = "call-process" |> Symbol.intern
  let call_process_region = "call-process-region" |> Symbol.intern
  let closed = "closed" |> Symbol.intern
  let connect = "connect" |> Symbol.intern
  let exit_ = "exit" |> Symbol.intern
  let failed = "failed" |> Symbol.intern
  let listen = "listen" |> Symbol.intern
  let make_pipe_process = "make-pipe-process" |> Symbol.intern
  let make_process = "make-process" |> Symbol.intern
  let open_ = "open" |> Symbol.intern
  let run = "run" |> Symbol.intern
  let signal = "signal" |> Symbol.intern
  let stop = "stop" |> Symbol.intern
end

include Process0

module Status = struct
  module T = struct
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
    [@@deriving enumerate, sexp_of]
  end

  include T

  include Valueable.Make (struct
      type nonrec t = t

      let type_ =
        Value.Type.enum
          [%sexp "process-status"]
          (module T)
          (Symbol.to_value
           << function
           | Closed -> Q.closed
           | Connect -> Q.connect
           | Exit -> Q.exit_
           | Failed -> Q.failed
           | Listen -> Q.listen
           | Open -> Q.open_
           | Run -> Q.run
           | Signal -> Q.signal
           | Stop -> Q.stop)
      ;;
    end)
end

let is_alive = Funcall.Wrap.("process-live-p" <: t @-> return bool)
let equal = eq
let buffer = Funcall.Wrap.("process-buffer" <: t @-> return (nil_or Buffer.t))
let process_command = Funcall.Wrap.("process-command" <: t @-> return value)

let command t =
  let v = process_command t in
  if Value.is_nil v || Value.eq v Value.t
  then None
  else Some (v |> Value.to_list_exn ~f:Value.to_utf8_bytes_exn)
;;

let name = Funcall.Wrap.("process-name" <: t @-> return string)
let process_id = Funcall.Wrap.("process-id" <: t @-> return (nil_or int))
let pid t = process_id t |> Option.map ~f:Pid.of_int
let mark = Funcall.Wrap.("process-mark" <: t @-> return Marker.t)
let query_on_exit = Funcall.Wrap.("process-query-on-exit-flag" <: t @-> return bool)

let set_query_on_exit =
  Funcall.Wrap.("set-process-query-on-exit-flag" <: t @-> bool @-> return nil)
;;

let get_property =
  Funcall.Wrap.("process-get" <: t @-> Symbol.t @-> return (nil_or value))
;;

let set_property = Funcall.Wrap.("process-put" <: t @-> Symbol.t @-> value @-> return nil)
let status = Funcall.Wrap.("process-status" <: t @-> return Status.t)

module Exit_status = struct
  type t =
    | Not_exited
    | Exited of int
    | Fatal_signal of int
  [@@deriving sexp]
end

let process_exit_status = Funcall.Wrap.("process-exit-status" <: t @-> return int)

let exit_status t : Exit_status.t =
  match status t with
  | Exit -> Exited (process_exit_status t)
  | Signal -> Fatal_signal (process_exit_status t)
  | Closed | Connect | Failed | Listen | Open | Run | Stop -> Not_exited
;;

let find_by_name = Funcall.Wrap.("get-process" <: string @-> return (nil_or t))
let all_emacs_children = Funcall.Wrap.("process-list" <: nullary @-> return (list t))

let create ?buffer ?coding ?(query_before_exit = true) ?stderr prog args ~name () =
  Symbol.funcallN
    Q.make_process
    (List.concat
       [ [ Q.K.name |> Symbol.to_value
         ; name |> Value.of_utf8_bytes
         ; Q.K.buffer |> Symbol.to_value
         ; (match buffer with
            | None -> Value.nil
            | Some b -> b |> Buffer.to_value)
         ; Q.K.command |> Symbol.to_value
         ; prog :: args |> List.map ~f:Value.of_utf8_bytes |> Value.list
         ; Q.K.noquery |> Symbol.to_value
         ; (if query_before_exit then Value.nil else Value.t)
         ]
       ; (match coding with
          | None -> []
          | Some coding -> [ Q.K.coding |> Symbol.to_value; coding |> Symbol.to_value ])
       ; (match stderr with
          | None -> []
          | Some stderr_buffer ->
            [ Q.K.stderr |> Symbol.to_value; stderr_buffer |> Buffer.to_value ])
       ])
  |> of_value_exn
;;

let kill = Funcall.Wrap.("kill-process" <: t @-> return nil)
let delete = Funcall.Wrap.("delete-process" <: t @-> return nil)

let make_pipe_process ?coding ~(here : [%call_pos]) () ~filter ~name ~noquery =
  of_value_exn
    (Symbol.funcallN
       Q.make_pipe_process
       ([ Q.K.name |> Symbol.to_value
        ; name |> Value.of_utf8_bytes
        ; Q.K.noquery |> Symbol.to_value
        ; noquery |> Value.of_bool
        ; Q.K.buffer |> Symbol.to_value
        ; Value.nil
        ; Q.K.filter |> Symbol.to_value
        ; Function.to_value
            (Defun.lambda
               here
               (Returns Value.Type.unit)
               (let%map_open.Defun () = return ()
                and process = required "process" t
                and output = required "output" Text.t in
                filter process output))
        ]
        @
        match coding with
        | None -> []
        | Some (`Decoding decoding, `Encoding encoding) ->
          [ Q.K.coding |> Symbol.to_value
          ; Value.cons (Coding_system.to_value decoding) (Coding_system.to_value encoding)
          ]))
;;

module Call = struct
  module Input = struct
    type t =
      | Dev_null
      | File of string
    [@@deriving sexp_of]

    let to_value = function
      | Dev_null -> Value.nil
      | File file -> file |> Value.of_utf8_bytes
    ;;
  end

  module Region_input = struct
    type t =
      | Region of
          { start : Position.t
          ; end_ : Position.t
          ; delete : bool
          }
      | String of string
    [@@deriving sexp_of]
  end

  module Output = struct
    module Stdout = struct
      type t =
        | Before_point_in of Buffer.t
        | Before_point_in_current_buffer
        | Dev_null
        | Overwrite_file of string
      [@@deriving sexp_of]

      let to_value = function
        | Before_point_in buffer -> buffer |> Buffer.to_value
        | Before_point_in_current_buffer -> Value.t
        | Dev_null -> Value.nil
        | Overwrite_file string ->
          Value.list [ Q.K.file |> Symbol.to_value; string |> Value.of_utf8_bytes ]
      ;;
    end

    module Stderr = struct
      type t =
        | Dev_null
        | Overwrite_file of string
      [@@deriving sexp_of]

      let to_value = function
        | Dev_null -> Value.nil
        | Overwrite_file string -> string |> Value.of_utf8_bytes
      ;;
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

    let to_value = function
      | Before_point_in buffer -> buffer |> Buffer.to_value
      | Before_point_in_current_buffer -> Value.t
      | Dev_null -> Value.nil
      | Overwrite_file string ->
        Value.list [ Q.K.file |> Symbol.to_value; string |> Value.of_utf8_bytes ]
      | Split { stderr; stdout } ->
        Value.list [ stdout |> Stdout.to_value; stderr |> Stderr.to_value ]
    ;;
  end

  module Result = struct
    type t =
      | Exit_status of int
      | Signaled of string
    [@@deriving sexp_of]

    let of_value_exn value =
      if Value.is_integer value
      then Exit_status (value |> Value.to_int_exn)
      else if Value.is_string value
      then Signaled (value |> Value.to_utf8_bytes_exn)
      else
        raise_s
          [%message
            "[Process.Call.Result.of_value_exn] got unexpected value" (value : Value.t)]
    ;;
  end
end

let call_region_exn
  ?(input =
    Call.Region_input.Region { start = Point.min (); end_ = Point.max (); delete = false })
  ?(output = Call.Output.Dev_null)
  ?(redisplay_on_output = false)
  ?(working_directory = Working_directory.Root)
  prog
  args
  =
  Working_directory.within working_directory Sync ~f:(fun () ->
    let start, end_, delete =
      match input with
      | Region { start; end_; delete } ->
        start |> Position.to_value, end_ |> Position.to_value, delete
      | String s -> s |> Value.of_utf8_bytes, Value.nil, false
    in
    Symbol.funcallN
      Q.call_process_region
      ([ start
       ; end_
       ; prog |> Value.of_utf8_bytes
       ; delete |> Value.of_bool
       ; output |> Call.Output.to_value
       ; redisplay_on_output |> Value.of_bool
       ]
       @ (args |> List.map ~f:Value.of_utf8_bytes))
    |> Call.Result.of_value_exn)
;;

let call_result_exn
  ?(input = Call.Input.Dev_null)
  ?(output = Call.Output.Dev_null)
  ?(redisplay_on_output = false)
  ?(working_directory = Working_directory.Root)
  prog
  args
  =
  Working_directory.within working_directory Sync ~f:(fun () ->
    Symbol.funcallN
      Q.call_process
      ([ prog |> Value.of_utf8_bytes
       ; input |> Call.Input.to_value
       ; output |> Call.Output.to_value
       ; redisplay_on_output |> Value.of_bool
       ]
       @ (args |> List.map ~f:Value.of_utf8_bytes))
    |> Call.Result.of_value_exn)
;;

module Lines_or_sexp = struct
  include Async_unix.Process.Lines_or_sexp

  let of_text text = text |> Text.to_utf8_bytes |> String.strip |> create
end

let call_exn
  ?input
  ?working_directory
  ?(strip_whitespace = true)
  ?(verbose_exn = true)
  ~stderr
  prog
  args
  =
  let strip_if_requested s = if strip_whitespace then String.strip s else s in
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    match
      call_result_exn
        prog
        args
        ?input
        ?working_directory
        ~output:
          (match stderr with
           | `Mix -> Before_point_in_current_buffer
           | `Split stderr -> Split { stdout = Before_point_in_current_buffer; stderr })
    with
    | Exit_status 0 ->
      let buffer_contents = Current_buffer.contents () |> Text.to_utf8_bytes in
      strip_if_requested buffer_contents
    | result ->
      let output = Current_buffer.contents () |> Lines_or_sexp.of_text in
      let stderr =
        match stderr with
        | `Mix -> None
        | `Split (Overwrite_file file : Call.Output.Stderr.t) ->
          (match In_channel.read_all file with
           | "" -> None
           | s -> Some (strip_if_requested s))
        | `Split (Dev_null : Call.Output.Stderr.t) -> None
      in
      (match verbose_exn with
       | true ->
         raise_s
           [%message
             "[Process.call_exn] failed"
               (prog : string)
               (args : string list)
               (result : Call.Result.t)
               (output : Lines_or_sexp.t)
               (stderr : (string option[@sexp.option]))]
       | false ->
         (match stderr with
          | None -> raise_s [%sexp (output : Lines_or_sexp.t)]
          | Some stderr -> raise_s [%sexp { output : Lines_or_sexp.t; stderr : string }])))
;;

let call_drop_stderr_if_ok_exn
  ?input
  ?working_directory
  ?strip_whitespace
  ?verbose_exn
  prog
  args
  =
  File.with_temp_file Sync ~prefix:"emacs" ~suffix:"process-stderr" ~f:(fun stderr_file ->
    let result =
      call_exn
        ~stderr:(`Split (Overwrite_file stderr_file : Call.Output.Stderr.t))
        ?input
        ?working_directory
        ?strip_whitespace
        ?verbose_exn
        prog
        args
    in
    (* Ignoring stderr in the OK case. Stderr will be printed by [call_exn] in the error
       case. *)
    result)
;;

let call_exn
  ?input
  ?working_directory
  ?strip_whitespace
  ?verbose_exn
  ?(stderr = `Mix)
  prog
  args
  =
  match stderr with
  | `Drop_if_ok ->
    call_drop_stderr_if_ok_exn
      ?input
      ?working_directory
      ?strip_whitespace
      ?verbose_exn
      prog
      args
  | (`Mix | `Split _) as stderr ->
    call_exn ?input ?working_directory ?strip_whitespace ?verbose_exn ~stderr prog args
;;

let call_expect_no_output_exn
  ?input
  ?working_directory
  ?(strip_whitespace = false)
  ?verbose_exn
  prog
  args
  =
  let result =
    call_exn ?input ?working_directory ?verbose_exn ~strip_whitespace prog args
  in
  if String.is_empty result
  then ()
  else
    raise_s
      [%message
        "[Process.call_expect_no_output_exn] produced unexpected output"
          (prog : string)
          (args : string list)
          (result : string)]
;;

let bash = "/bin/bash"

let shell_command_result ?input ?output ?redisplay_on_output ?working_directory command =
  call_result_exn
    bash
    [ "-c"; command ]
    ?input
    ?output
    ?redisplay_on_output
    ?working_directory
;;

let shell_command_exn ?input ?working_directory ?verbose_exn command =
  call_exn bash [ "-c"; command ] ?input ?working_directory ?verbose_exn
;;

let shell_command_expect_no_output_exn ?input ?working_directory ?verbose_exn command =
  call_expect_no_output_exn bash [ "-c"; command ] ?input ?working_directory ?verbose_exn
;;

let process_sentinel =
  Funcall.Wrap.("process-sentinel" <: t @-> return (nil_or Function.t))
;;

let set_process_sentinel =
  Funcall.Wrap.("set-process-sentinel" <: t @-> Function.t @-> return nil)
;;

let wrap_sentinel
  (type a)
  here
  t
  (returns : (unit, a) Defun.Returns.t)
  ~(sentinel : event:string -> a)
  ~event
  =
  let log_exn exn =
    message_s
      [%message
        "process sentinel raised"
          ~sentinel_created_at:(here : Source_code_position.t)
          ~process:(t : t)
          (exn : exn)]
  in
  match returns with
  | Returns _ ->
    ((try sentinel ~event with
      | exn -> log_exn exn)
     : a)
  | Returns_deferred _ ->
    (match%map Monitor.try_with ~extract_exn:true (fun () -> sentinel ~event) with
     | Ok () -> ()
     | Error exn -> log_exn exn)
;;

let set_sentinel
  (type a)
  ~(here : [%call_pos])
  t
  (returns : (unit, a) Defun.Returns.t)
  ~(sentinel : event:string -> a)
  =
  let sentinel = wrap_sentinel here t returns ~sentinel in
  set_process_sentinel
    t
    (Defun.lambda
       here
       returns
       (let%map_open.Defun () = return ()
        and _process = required "process" type_
        and event = required "event" string in
        sentinel ~event))
;;

let extend_sentinel
  (type a)
  ~(here : [%call_pos])
  t
  (returns : (unit, a) Defun.Returns.t)
  ~(sentinel : event:string -> a)
  =
  let sentinel = wrap_sentinel here t returns ~sentinel in
  let previous_sentinel = process_sentinel t in
  set_process_sentinel
    t
    (Defun.lambda
       here
       returns
       (let%map_open.Defun () = return ()
        and process = required "process" value
        and event = required "event" value in
        let run_previous_sentinel () =
          match previous_sentinel with
          | None -> ()
          | Some previous_sentinel ->
            Value.funcall2_i (previous_sentinel |> Function.to_value) process event
        in
        match returns with
        | Returns _ ->
          run_previous_sentinel ();
          Background.Private.mark_running_in_background (fun () : a ->
            sentinel ~event:(event |> Value.to_utf8_bytes_exn))
        | Returns_deferred _ ->
          let%bind.Deferred () =
            Value.Private.run_outside_async
              ~allowed_in_background:true
              run_previous_sentinel
          in
          Background.Private.mark_running_in_background (fun () ->
            sentinel ~event:(event |> Value.to_utf8_bytes_exn))))
;;

module Exited = struct
  type t =
    | Exited of int
    | Fatal_signal of int
  [@@deriving sexp_of]

  let successfully = function
    | Exited 0 -> true
    | Exited _ | Fatal_signal _ -> false
  ;;
end

let exited =
  let property = "exited" |> Symbol.intern in
  let type_ =
    Caml_embed.create_type
      (Type_equal.Id.create ~name:"exited" [%sexp_of: Exited.t Deferred.t])
  in
  fun ~(here : [%call_pos]) t ->
    let check_status () : Exited.t option =
      match exit_status t with
      | Not_exited -> None
      | Exited i -> Some (Exited i)
      | Fatal_signal i -> Some (Fatal_signal i)
    in
    match get_property t property with
    | Some v -> v |> Value.Type.of_value_exn type_
    | None ->
      (match check_status () with
       | Some x -> return x
       | None ->
         let ivar : Exited.t Ivar.t = Ivar.create () in
         extend_sentinel ~here t (Returns Value.Type.unit) ~sentinel:(fun ~event:_ ->
           Option.iter (check_status ()) ~f:(Ivar.fill_if_empty ivar));
         let exited = Ivar.read ivar in
         set_property t property (exited |> Value.Type.to_value type_);
         exited)
;;
