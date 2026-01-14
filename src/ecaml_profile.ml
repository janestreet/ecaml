open! Core
open! Async_kernel
open! Import

let print_length =
  Customization.defcustom
    ("ecaml-profile-print-length" |> Symbol.intern)
    [%here]
    ~docstring:
      {|
Use this as `print-length' for converting Elisp values to sexps in profile records.
|}
    ~group:Customization.Group.ecaml
    ~type_:Value.Type.(nil_or int)
    ~customization_type:Integer
    ~standard_value:(Some 10)
    ~on_set:(fun o -> Value.Private.ecaml_profile_print_length := o)
    ()
;;

let print_level =
  Customization.defcustom
    ("ecaml-profile-print-level" |> Symbol.intern)
    [%here]
    ~docstring:
      {|
Use this as `print-level' for converting Elisp values to sexps in profile records.
|}
    ~group:Customization.Group.ecaml
    ~type_:Value.Type.(nil_or int)
    ~customization_type:Integer
    ~standard_value:(Some 10)
    ~on_set:(fun o -> Value.Private.ecaml_profile_print_level := o)
    ()
;;

let profile_kill_buffer_query_function =
  Hook.Function.create
    ("ecaml-profile-kill-buffer-query-function" |> Symbol.intern)
    [%here]
    ~docstring:
      {|
Prompt the user before killing the *profile* buffer and losing data.
|}
    ~hook_type:Query_function
    (Returns_deferred Value.Type.bool)
    (fun () ->
       if am_running_test
       then return true
       else
         Minibuffer.yes_or_no
           ~prompt:
             (Documentation.O.(
                substitute_command_keys
                  [%string
                    {|Killing the profile buffer loses data, and is probably not what you meant.

Consider pressing \[quit-window] instead.

Kill the profile buffer? |}])
              |> Text.to_utf8_bytes))
;;

let major_mode =
  Major_mode.define_derived_mode
    ("ecaml-profile-mode" |> Symbol.intern)
    [%here]
    ~docstring:
      {|
The major mode for the *profile* buffer, which holds a log of Ecaml profile output.
|}
    ~parent:Major_mode.special
    ~mode_line:"ecaml-profile"
    ~initialize:
      ( Returns Value.Type.unit
      , fun () ->
          Hook.add_local
            Buffer.kill_buffer_query_functions
            profile_kill_buffer_query_function )
    ()
;;

module Start_location = struct
  include Profile.Start_location

  let docstring =
    Option.some
    << function
    | End_of_profile_first_line -> "put the time at the end of the profile's first line"
    | Line_preceding_profile -> "put the time on its own line, before the profile"
  ;;
end

module Profile_buffer : sig
  val profile_buffer : unit -> Buffer.t option
end = struct
  type t =
    | Absent
    | Initializing
    | This of Buffer.t
  [@@deriving sexp_of]

  let profile_buffer : t ref = ref Absent

  let initialize_profile_buffer () =
    profile_buffer := Initializing;
    let buffer = Buffer.create ~name:"*profile*" in
    Major_mode.Blocking.change_to major_mode ~in_:buffer;
    Buffer_local.set Current_buffer.truncate_lines true buffer;
    (* Force it to be in ~ to avoid inheriting the default-directory from wherever we
       first called into the profiler. *)
    Buffer_local.set Current_buffer.directory (Some "~/") buffer;
    profile_buffer := This buffer;
    Some buffer
  ;;

  let profile_buffer () =
    match !profile_buffer with
    | Initializing -> None
    | Absent -> initialize_profile_buffer ()
    | This buffer ->
      if Buffer.is_live buffer then Some buffer else initialize_profile_buffer ()
  ;;
end

let tag_function =
  Defvar.defvar
    ("ecaml-profile-tag-frame-function" |> Symbol.intern)
    [%here]
    ~docstring:
      {|
A function of no arguments called when creating a profile frame.

The return value of this function is added to the profile frame, to
provide additional context (e.g., the current buffer).
|}
    ~type_:(Value.Type.nil_or Function.type_)
    ~initial_value:None
    ()
;;

let () =
  let (_ : _ Customization.t) =
    Customization.defcustom_enum
      ("ecaml-profile-start-location" |> Symbol.intern)
      [%here]
      (module Start_location)
      ~docstring:{| Where to render a profile's start time: |}
      ~group:Customization.Group.ecaml
      ~standard_value:End_of_profile_first_line
      ~on_set:(fun start_location -> Profile.start_location := start_location)
      ()
  in
  let should_profile =
    Customization.defcustom
      ("ecaml-profile-should-profile" |> Symbol.intern)
      [%here]
      ~docstring:{| Whether profiling is enabled. |}
      ~group:Customization.Group.ecaml
      ~type_:Value.Type.bool
      ~customization_type:Boolean
      ~standard_value:true
      ~on_set:(fun bool -> Profile.should_profile := bool)
      ()
  in
  let _hide_if_less_than =
    Customization.defcustom
      ("ecaml-profile-hide-frame-if-less-than" |> Symbol.intern)
      [%here]
      ~docstring:{| Hide profile frames shorter than this duration. |}
      ~group:Customization.Group.ecaml
      ~type_:Value.Type.string
      ~customization_type:String
      ~standard_value:"1ms"
      ~on_set:(fun string ->
        Profile.hide_if_less_than := string |> Time_ns.Span.of_string)
      ()
  in
  let _hide_top_level_if_less_than =
    Customization.defcustom
      ("ecaml-profile-hide-top-level-if-less-than" |> Symbol.intern)
      [%here]
      ~docstring:{| Hide profiles shorter than this duration. |}
      ~group:Customization.Group.ecaml
      ~type_:Value.Type.string
      ~customization_type:String
      ~standard_value:"100ms"
      ~on_set:(fun string ->
        Profile.hide_top_level_if_less_than := string |> Time_ns.Span.of_string)
      ()
  in
  (Profile.Private.on_async_out_of_order
   := fun (lazy sexp) -> Echo_area.inhibit_messages Sync (fun () -> message_s sexp));
  (Profile.sexp_of_time_ns
   := fun time_ns ->
        match [%sexp (time_ns : Time_ns_unix.t)] with
        | List [ date; ofday ] -> List [ ofday; date ]
        | sexp -> sexp);
  (Profile.output_profile
   := fun string ->
        (* If [output_profile] raises, then Nested_profile use [eprint_s] to print the
           exception, which doesn't work well in Emacs. So we do our own exception
           handling. *)
        try
          match Profile_buffer.profile_buffer () with
          | None -> ()
          | Some buffer ->
            Current_buffer.set_temporarily Sync buffer ~f:(fun () ->
              Current_buffer.inhibit_read_only Sync (fun () ->
                Current_buffer.append_to string))
        with
        | exn -> message_s [%message "unable to output profile" ~_:(exn : exn)]);
  Profile.tag_frames_with
  := Some
       (T
          { capture =
              (fun () ->
                match Current_buffer.value_exn tag_function with
                | None -> None
                | Some f -> Some (f |> Function.to_value |> Value.funcall0))
          ; render = Option.return << [%sexp_of: Value.t]
          });
  Hook.add
    Elisp_gc.post_gc_hook
    (Hook.Function.create
       ("ecaml-profile-record-gc" |> Symbol.intern)
       [%here]
       ~docstring:
         {|
Internal to the Ecaml profiler.

Called by `post-gc-hook' to add Elisp GC information to the Ecaml profiler.
|}
         (* We don't profile this hook so that the gc frame is attributed to the enclosing
            frame that actually experienced the gc. *)
       ~should_profile:false
       ~hook_type:Normal_hook
       (Returns Value.Type.unit)
       (let last_gc_elapsed = ref (Elisp_gc.gc_elapsed ()) in
        fun () ->
          if !Profile.should_profile
          then (
            let module Clock = Profile.Private.Clock in
            let clock = !Profile.Private.clock in
            let gc_elapsed = Elisp_gc.gc_elapsed () in
            let gc_took =
              if Clock.is_virtual clock
              then (
                (* A fixed time, to make test output deterministic. *)
                let took = 10 |> Time_ns.Span.of_int_ms in
                Clock.advance clock ~by:took;
                took)
              else Time_ns.Span.( - ) gc_elapsed !last_gc_elapsed
            in
            last_gc_elapsed := gc_elapsed;
            let stop = Clock.now clock in
            let gcs_done =
              Ref.set_temporarily Profile.should_profile false ~f:Elisp_gc.gcs_done
            in
            Profile.Private.record_frame
              ~start:(Time_ns.sub stop gc_took)
              ~stop
              ~message:[%lazy_message "gc" (gcs_done : int opaque_in_test)])));
  let set_should_profile bool =
    Customization.set_value should_profile bool;
    let verb = if !Profile.should_profile then "enabled" else "disabled" in
    message [%string "You just %{verb} ecaml profiling"]
  in
  Defun.defun_nullary_nil
    ("ecaml-toggle-profiling" |> Symbol.intern)
    [%here]
    ~docstring:
      "Enable or disable logging of elisp->ecaml calls and durations in the `*profile*' \
       buffer."
    ~interactive:No_arg
    (fun () -> set_should_profile (not !Profile.should_profile));
  Defun.defun_nullary_nil
    ("ecaml-enable-profiling" |> Symbol.intern)
    [%here]
    ~docstring:
      "Enable logging of elisp->ecaml calls and durations in the `*profile*' buffer."
    ~interactive:No_arg
    (fun () -> set_should_profile true);
  Defun.defun_nullary_nil
    ("ecaml-disable-profiling" |> Symbol.intern)
    [%here]
    ~docstring:
      "Disable logging of elisp->ecaml calls and durations in the `*profile*' buffer."
    ~interactive:No_arg
    (fun () -> set_should_profile false);
  Defun.defun
    ("ecaml-profile--inner" |> Symbol.intern)
    [%here]
    ~docstring:
      {|
Call FUNCTION with ARGS inside a call to [Nested_profile.profile].

FUNCTION-NAME should be the name of the function, to be used when
rendering the profile.  This is passed separately because advice
combinators receive the function definition and not its name as
input.
|}
    ~should_profile:false
    (Returns Value.Type.value)
    (let%map_open.Defun () = return ()
     and function_name = required "function-name" Symbol.t
     and f = required "function" value
     and args = rest "args" value in
     profile
       Sync
       [%lazy_sexp (Symbol.to_value function_name :: args : Value.t list)]
       (fun () -> Value.funcallN ~should_profile:false f args));
  Defun.defun_nullary
    ("ecaml-profile-test-parallel-profile" |> Symbol.intern)
    [%here]
    ~docstring:
      {|
For testing the Ecaml profiler.

Test how the Ecaml profiler handles two Async jobs running in parallel.
|}
    (Returns_deferred Value.Type.unit)
    (fun () ->
       profile Async [%lazy_sexp "The whole thing"] (fun () ->
         let%bind () =
           profile Async [%lazy_sexp "branch1"] (fun () ->
             Clock_ns.after (Time_ns.Span.of_sec 1.))
         and () =
           profile Async [%lazy_sexp "branch2"] (fun () ->
             Clock_ns.after (Time_ns.Span.of_sec 0.8))
         in
         return ()))
;;

[@@@warning "-unused-module"]

module Benchmarks = struct
  let helper_name = "ecaml-profile-benchmark-rendering-helper"

  let () =
    Defun.defun
      (helper_name |> Symbol.intern)
      [%here]
      ~docstring:
        {|
For testing the Ecaml profiler.

Benchmark the Ecaml profiler's rendering of a large value.
|}
      (Returns Value.Type.unit)
      (let%map_open.Defun () = return ()
       and _ignored = required "large-data-structure" value in
       ())
  ;;

  let make_benchmark ~name ~create =
    Defun.defun_nullary_nil
      ("ecaml-profile-benchmark-rendering-" ^ name |> Symbol.intern)
      [%here]
      ~docstring:
        [%string
          {|
For testing the Ecaml profiler.

Benchmark the Ecaml profiler's rendering of a large %{name}.
|}]
      ~interactive:No_arg
      (fun () ->
        (* We [Profile.disown] because we want to render the profile below under the
           settings of our own choosing, rather than have it incorporated into the profile
           of the outer command. *)
        Profile.disown (fun () ->
          let data = create () in
          Ref.set_temporarily Profile.should_profile true ~f:(fun () ->
            Ref.set_temporarily Profile.hide_if_less_than Time_ns.Span.zero ~f:(fun () ->
              Ref.set_temporarily
                Profile.hide_top_level_if_less_than
                Time_ns.Span.zero
                ~f:(fun () -> Funcall.Wrap.(helper_name <: value @-> return nil) data)))))
  ;;

  let large_string = lazy (String.make 1024 'a' |> Value.of_utf8_bytes)
  let copies_of_string = 1024 * 64

  let () =
    make_benchmark ~name:"hash-table" ~create:(fun () ->
      let large_hash_table = Hash_table.create () in
      for i = 0 to copies_of_string do
        Hash_table.set
          large_hash_table
          ~key:(Value.of_int_exn i)
          ~data:(force large_string)
      done;
      large_hash_table |> Hash_table.to_value)
  ;;

  let () =
    make_benchmark ~name:"list" ~create:(fun () ->
      List.init copies_of_string ~f:(fun _ -> force large_string)
      |> Value.Type.(to_value (list value)))
  ;;

  let () =
    make_benchmark ~name:"vector" ~create:(fun () ->
      Vector.of_list (List.init copies_of_string ~f:(fun _ -> force large_string))
      |> Vector.to_value)
  ;;
end

module Private = struct
  let tag_function = tag_function
end
