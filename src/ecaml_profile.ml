open! Core_kernel
open! Async_kernel
open! Import

include (val Major_mode.define_derived_mode
               ("ecaml-profile-mode" |> Symbol.intern)
               [%here]
               ~docstring:
                 {|
The major mode for the *profile* buffer, which holds a log of Ecaml profile output.
|}
               ~define_keys:
                 [ "SPC", "scroll-up" |> Symbol.intern
                 ; "DEL", "scroll-down" |> Symbol.intern
                 ; "<", "beginning-of-buffer" |> Symbol.intern
                 ; ">", "end-of-buffer" |> Symbol.intern
                 ; "q", "quit-window" |> Symbol.intern
                 ]
               ~mode_line:"ecaml-profile"
               ~initialize:
                 ( Returns Value.Type.unit
                 , fun () -> Minor_mode.enable Minor_mode.read_only )
               ())

let () = Keymap.suppress_keymap (Major_mode.keymap major_mode) ~suppress_digits:true

module Start_location = struct
  module T = struct
    include Profile.Start_location

    let to_string t =
      [%sexp (t : t)]
      |> Sexp.to_string
      |> String.lowercase
      |> String.tr ~target:'_' ~replacement:'-'
    ;;

    let to_symbol t = t |> to_string |> Symbol.intern

    let docstring = function
      | End_of_profile_first_line ->
        "put the time at the end of the profile's first line"
      | Line_preceding_profile -> "put the time on its own line, before the profile"
    ;;
  end

  include T

  let customization =
    Customization.defcustom_enum
      ("ecaml-profile-start-location" |> Symbol.intern)
      [%here]
      (module T)
      ~docstring:{|
Where to render a profile's start time:
|}
      ~group:Customization.Group.ecaml
      ~standard_value:End_of_profile_first_line
      ()
  ;;

  let () = Profile.start_location := Customization.value customization
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
    Background.don't_wait_for [%here] (fun () ->
      let%bind () = Major_mode.change_to major_mode ~in_:buffer in
      Buffer_local.set Current_buffer.truncate_lines true buffer;
      profile_buffer := This buffer;
      return ())
  ;;

  let profile_buffer () =
    match !profile_buffer with
    | Initializing -> None
    | Absent ->
      initialize_profile_buffer ();
      None
    | This buffer ->
      if Buffer.is_live buffer
      then Some buffer
      else (
        initialize_profile_buffer ();
        None)
  ;;
end

let set_should_profile should_profile =
  Profile.should_profile := should_profile;
  let verb = if !Profile.should_profile then "enabled" else "disabled" in
  message (String.concat [ "You just "; verb; " ecaml profiling" ])
;;

let initialize () =
  (* When not in test, we start initializing the profile buffer, so that it exists when we
     need it. *)
  if not am_running_test then ignore (Profile_buffer.profile_buffer () : Buffer.t option);
  (Profile.sexp_of_time_ns
   := fun time_ns ->
     match [%sexp (time_ns : Core.Time_ns.t)] with
     | List [ date; ofday ] -> List [ ofday; date ]
     | sexp -> sexp);
  (Profile.output_profile
   := fun string ->
     (* If [output_profile] raises, then Nested_profile use [eprint_s] to print the
        exception, which doesn't work well in Emacs.  So we do our own exception
        handling. *)
     try
       match Profile_buffer.profile_buffer () with
       | None -> ()
       | Some buffer ->
         Current_buffer.set_temporarily buffer Sync ~f:(fun () ->
           Current_buffer.inhibit_read_only Sync (fun () ->
             Current_buffer.append_to string))
     with
     | exn -> message_s [%message "unable to output profile" ~_:(exn : exn)]);
  Hook.add
    Elisp_gc.post_gc_hook
    (Hook.Function.create
       ("ecaml-profile-record-gc" |> Symbol.intern)
       [%here]
       (* We don't profile this hook so that the gc frame is attributed to the enclosing
          frame that actually experienced the gc. *)
       ~should_profile:false
       ~hook_type:Normal
       (Returns Value.Type.unit)
       (let last_gc_elapsed = ref (Elisp_gc.gc_elapsed ()) in
        fun () ->
          if !Profile.should_profile
          then (
            let module Clock = Profile.Private.Clock in
            let clock = !Profile.Private.clock in
            let gc_elapsed = Elisp_gc.gc_elapsed () in
            let gc_took =
              if not am_running_test
              then Time_ns.Span.( - ) gc_elapsed !last_gc_elapsed
              else (
                (* A fixed time, to make test output deterministic. *)
                let took = 10 |> Time_ns.Span.of_int_ms in
                Clock.advance clock ~by:took;
                took)
            in
            last_gc_elapsed := gc_elapsed;
            let stop = Clock.now clock in
            let gcs_done =
              Ref.set_temporarily Profile.should_profile false ~f:Elisp_gc.gcs_done
            in
            Profile.Private.record_frame
              ~start:(Time_ns.sub stop gc_took)
              ~stop
              ~message:(lazy [%message "gc" (gcs_done : int opaque_in_test)]))));
  let open Defun in
  defun_nullary_nil
    ("ecaml-toggle-profiling" |> Symbol.intern)
    [%here]
    ~docstring:
      "Enable or disable logging of elisp->ecaml calls and durations in the `*profile*' \
       buffer."
    ~interactive:No_arg
    (fun () -> set_should_profile (not !Profile.should_profile));
  defun_nullary_nil
    ("ecaml-enable-profiling" |> Symbol.intern)
    [%here]
    ~docstring:
      "Enable logging of elisp->ecaml calls and durations in the `*profile*' buffer."
    ~interactive:No_arg
    (fun () -> set_should_profile true);
  defun_nullary_nil
    ("ecaml-disable-profiling" |> Symbol.intern)
    [%here]
    ~docstring:
      "Disable logging of elisp->ecaml calls and durations in the `*profile*' buffer."
    ~interactive:No_arg
    (fun () -> set_should_profile false);
  let defun_set_span name setting =
    defun
      (concat [ "ecaml-profile-set-"; name ] |> Symbol.intern)
      [%here]
      ~docstring:
        (concat
           [ "Don't record profiles shorter than this duration (default is "
           ; !setting |> Time_ns.Span.to_short_string
           ; ")"
           ])
      ~interactive:
        (Prompt
           (concat
              [ "set "
              ; name
              ; " duration (currently "
              ; !setting |> Time_ns.Span.to_short_string
              ; "): "
              ]))
      (Returns Value.Type.unit)
      (let%map.Let_syntax duration =
         required ("duration" |> Symbol.intern) Value.Type.string
       in
       setting := Time_ns.Span.of_string duration)
  in
  defun_set_span "hide-if-less-than" Profile.hide_if_less_than;
  defun_set_span "hide-top-level-if-less-than" Profile.hide_top_level_if_less_than;
  defun
    ("ecaml-profile-inner" |> Symbol.intern)
    [%here]
    ~docstring:"profile an elisp function using Nested_profile"
    ~should_profile:false
    (Returns Value.Type.value)
    (let%map_open.Defun.Let_syntax () = return ()
     and description = required ("DESCRIPTION" |> Symbol.intern) Value.Type.string
     and f = required ("FUNCTION" |> Symbol.intern) Value.Type.value in
     profile Sync (lazy [%message description]) (fun () -> Value.funcall0 f));
  defun
    ("ecaml-profile-elisp-function" |> Symbol.intern)
    [%here]
    ~docstring:"Wrap the given function in a call to [Nested_profile.profile]."
    ~interactive:(Function_name { prompt = "Profile function: " })
    (Returns Value.Type.unit)
    (let%map_open.Defun.Let_syntax () = return ()
     and fn = required ("function" |> Symbol.intern) Symbol.type_ in
     Advice.around_values
       ("ecaml-profile-wrapper-for-" ^ Symbol.name fn |> Symbol.intern)
       [%here]
       ~for_function:fn
       ~should_profile:false
       (fun f args ->
          profile
            Sync
            (lazy [%sexp ((fn |> Symbol.to_value) :: args : Value.t list)])
            (fun () -> f args)))
;;
