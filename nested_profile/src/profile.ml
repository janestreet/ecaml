open! Core_kernel
open! Async_kernel

module Start_location = struct
  type t =
    | Line_preceding_profile
    | End_of_profile_first_line
  [@@deriving compare, enumerate, sexp_of]

  let default = End_of_profile_first_line
end

let start_location = ref Start_location.default
let concat = String.concat
let approximate_line_length_limit = ref 1_000
let should_profile = ref false
let hide_if_less_than = ref (Time_ns.Span.of_int_us 100)
let hide_top_level_if_less_than = ref (Time_ns.Span.of_int_ms 10)
let output_profile = ref print_string
let sexp_of_time_ns = ref [%sexp_of: Time_ns.Alternate_sexp.t]
let tag_frames_with = ref None

module Time_ns = struct
  include Time_ns

  let sexp_of_t t =
    try !sexp_of_time_ns t with
    | exn ->
      let backtrace = Backtrace.Exn.most_recent () in
      [%message "[Profile.sexp_of_time_ns] raised" (exn : exn) (backtrace : Backtrace.t)]
  ;;
end

module Clock = struct
  type t =
    | Wall
    | Virtual of { mutable now : Time_ns.t }
  [@@deriving sexp_of]

  let create ~now = Virtual { now }

  let now = function
    | Wall -> Time_ns.now ()
    | Virtual { now } -> now
  ;;

  let advance t ~by =
    match t with
    | Wall -> raise_s [%message "[Nested_profile.Clock.advance]"]
    | Virtual s -> s.now <- Time_ns.add s.now by
  ;;
end

let clock = ref Clock.Wall
let now () = Clock.now !clock

(* We don't support profiling for brief periods when profiler code calls user code,
   because doing so would be hard and could cause infinite regress, e.g. if that code in
   turns asks to be profiled.  So, we have an internal bool ref, [profiling_is_allowed],
   that we use to disable profiling when calling user code. *)
let profiling_is_allowed = ref true
let with_profiling_disallowed f = Ref.set_temporarily profiling_is_allowed false ~f

module Elide_in_test = struct
  type 'a t = 'a

  let sexp_of_t sexp_of_a a =
    if am_running_inline_test then [%message "<elided-in-test>"] else sexp_of_a a
  ;;
end

module Message : sig
  type t [@@deriving sexp_of]

  val create : Sexp.t Lazy.t -> t
  val am_forcing : unit -> bool
  val force : t -> Sexp.t
end = struct
  type t = Sexp.t Lazy.t [@@deriving sexp_of]

  let create t = t
  let am_forcing_ref = ref false
  let am_forcing () = !am_forcing_ref
  let force t = Ref.set_temporarily am_forcing_ref true ~f:(fun () -> force t)
end

let am_forcing_message = Message.am_forcing

module Record = struct
  type t =
    { start : Time_ns.t
    ; stop : Time_ns.t
    ; message : Message.t
    ; children : t list
    ; had_parallel_children : bool
    ; pending_children : int
    }

  let took t = Time_ns.diff t.stop t.start

  let rec sexp_of_t
            ({ start = _
             ; stop = _
             ; message
             ; children
             ; had_parallel_children
             ; pending_children
             } as t)
    =
    [%sexp
      (took t |> Time_ns.Span.to_string_hum : string)
    , (if had_parallel_children then Some `parallel else None
                                                         : ([ `parallel ] option[@sexp.option]))
    , (if pending_children <> 0
       then Some (`pending_children pending_children)
       else None
            : ([ `pending_children of int ] option[@sexp.option]))
    , (Message.force message : Sexp.t)
    , (children : (t list[@sexp.omit_nil]))]
  ;;

  let sexp_to_string_on_one_line =
    let buffer = Buffer.create 0 in
    let emit string = Buffer.add_string buffer string in
    let over_the_limit = ref false in
    let can_emit additional =
      (not !over_the_limit)
      &&
      (if Buffer.length buffer + additional > !approximate_line_length_limit
       then (
         over_the_limit := true;
         emit "...");
       not !over_the_limit)
    in
    let rec emit_sexp (sexp : Sexp.t) =
      match sexp with
      | Atom _ as sexp ->
        let string = Sexp.to_string sexp in
        if can_emit (String.length string) then emit string
      | List sexps ->
        if can_emit 2
        then (
          emit "(";
          (match sexps with
           | [] -> ()
           | sexp :: sexps ->
             emit_sexp sexp;
             List.iter sexps ~f:(fun sexp ->
               if not !over_the_limit
               then (
                 emit " ";
                 emit_sexp sexp)));
          emit ")")
    in
    fun sexp ->
      Buffer.clear buffer;
      over_the_limit := false;
      emit_sexp sexp;
      Buffer.contents buffer
  ;;

  let time_span_as_micros_with_two_digits_of_precision span =
    let span = span |> Time_ns.Span.to_int_us in
    let digits = String.length (span |> Int.to_string) in
    let precision = 2 in
    let microseconds =
      if digits <= precision
      then span
      else (
        let scale = Int.pow 10 (digits - precision) in
        scale
        * ((span |> Int.to_float) /. (scale |> Int.to_float) |> Float.iround_nearest_exn))
    in
    concat [ microseconds |> Int.to_string_hum; "us" ]
  ;;

  let pad_left string ~total_width =
    let n = String.length string in
    if n > total_width
    then string
    else concat [ String.make (total_width - n) ' '; string ]
  ;;

  let took_width t =
    String.length (took t |> time_span_as_micros_with_two_digits_of_precision)
  ;;

  let rec max_took_width t =
    Int.max
      (took_width t)
      (List.fold t.children ~init:0 ~f:(fun ac child ->
         Int.max ac (max_took_width child)))
  ;;

  let rec insert_gap_frames t =
    if List.is_empty t.children
    then t
    else (
      let maybe_add_gap ts ~start ~stop =
        let gap_took = Time_ns.diff stop start in
        (* We hide the gap frame if it took less than [!hide_if_less_than], like all other
           frames.  We also hide the gap frame if it took less than 1us, since a gap frame
           that says 0us would be noise. *)
        if Time_ns.Span.( < ) gap_took !hide_if_less_than
        || Time_ns.Span.( < ) gap_took Time_ns.Span.microsecond
        then ts
        else
          { start
          ; stop
          ; message = Message.create (lazy [%sexp "gap"])
          ; children = []
          ; had_parallel_children = false
          ; pending_children = 0
          }
          :: ts
      in
      let last_stop, rev_children =
        List.fold
          t.children
          ~init:(t.start, [])
          ~f:(fun (last_stop, rev_children) child ->
            ( child.stop
            , insert_gap_frames child
              :: maybe_add_gap rev_children ~start:last_stop ~stop:child.start ))
      in
      let rev_children = maybe_add_gap rev_children ~start:last_stop ~stop:t.stop in
      { t with children = List.rev rev_children })
  ;;

  let to_string_hum t =
    let rendering_started = now () in
    let start_location = !start_location in
    let t = insert_gap_frames t in
    let took_total_width = max_took_width t in
    let paren strings = concat [ "("; concat strings; ")" ] in
    let shift_right =
      match start_location with
      | End_of_profile_first_line -> 0
      | Line_preceding_profile -> 1
    in
    let start = [%sexp (t.start : Time_ns.t)] |> Sexp.to_string in
    let rec loop
              ({ message; children; had_parallel_children; pending_children; _ } as t)
              ~depth
              ~parent_took
      =
      let took = took t in
      let percentage =
        match parent_took with
        | None -> ""
        | Some parent_took ->
          let percentage_int =
            (if Time_ns.Span.equal parent_took Time_ns.Span.zero
             then "_"
             else
               Time_ns.Span.( // ) took parent_took *. 100.
               |> Float.iround_nearest_exn
               |> Int.to_string)
            |> pad_left ~total_width:3
          in
          concat [ percentage_int; "% " ]
      in
      let message =
        with_profiling_disallowed (fun () ->
          try Message.force message with
          | exn ->
            let backtrace = Backtrace.Exn.most_recent () in
            [%message
              "[Profile.profile] message raised" (exn : exn) (backtrace : Backtrace.t)])
      in
      concat
        [ String.make (shift_right + (3 * depth)) ' '
        ; paren
            [ percentage
            ; took
              |> time_span_as_micros_with_two_digits_of_precision
              |> pad_left ~total_width:took_total_width
            ; " "
            ; (if had_parallel_children then "[parallel] " else "")
            ; (match pending_children with
               | 0 -> ""
               | 1 -> "[1 pending child] "
               | n -> sprintf "[%d pending children] " n)
            ; message |> sexp_to_string_on_one_line
            ; (match start_location with
               | Line_preceding_profile -> ""
               | End_of_profile_first_line ->
                 if depth = 0 then concat [ " "; start ] else "")
            ; (if List.is_empty children
               then ""
               else
                 concat
                   [ " "
                   ; paren
                       [ "\n"
                       ; concat
                           ~sep:"\n"
                           (List.map
                              children
                              ~f:(loop ~depth:(depth + 1) ~parent_took:(Some took)))
                       ]
                   ])
            ]
        ]
    in
    let profile = loop t ~depth:0 ~parent_took:None in
    let rendering_finished = now () in
    let rendering_took = Time_ns.diff rendering_finished rendering_started in
    let rendering_took =
      if Time_ns.Span.( < ) rendering_took !hide_top_level_if_less_than
      then None
      else
        Some
          (paren
             [ "rendering_took "
             ; rendering_took |> time_span_as_micros_with_two_digits_of_precision
             ])
    in
    match start_location, rendering_took with
    | End_of_profile_first_line, None -> profile
    | End_of_profile_first_line, Some r -> paren [ r; "\n "; profile ]
    | Line_preceding_profile, None -> paren [ start; "\n"; profile ]
    | Line_preceding_profile, Some r -> paren [ start; "\n "; r; "\n"; profile ]
  ;;
end

module Frame = struct
  type t =
    { message : Message.t
    ; start : Time_ns.Alternate_sexp.t Elide_in_test.t
    ; children : Record.t Queue.t
    ; parent : t option
    ; mutable pending_children : int
    ; mutable max_pending_children : int
    }
  [@@deriving sexp_of]

  let create ~message ~parent =
    { message = Message.create message
    ; start = now ()
    ; children = Queue.create ()
    ; parent
    ; pending_children = 0
    ; max_pending_children = 0
    }
  ;;

  let record
        { message; start; children; parent = _; pending_children; max_pending_children }
        ~stop
    : Record.t
    =
    { start
    ; stop
    ; message
    ; children = children |> Queue.to_list
    ; had_parallel_children = max_pending_children > 1
    ; pending_children
    }
  ;;
end

module Profile_context = struct
  let record_profile frame ~stop =
    let record = Frame.record frame ~stop in
    match frame.parent with
    | None ->
      if Time_ns.Span.( >= ) (Record.took record) !hide_top_level_if_less_than
      then (
        let profile = concat [ record |> Record.to_string_hum; "\n" ] in
        with_profiling_disallowed (fun () ->
          try !output_profile profile with
          | exn ->
            let backtrace = Backtrace.Exn.most_recent () in
            eprint_s
              [%message
                "[Profile.output_profile] raised" (exn : exn) (backtrace : Backtrace.t)]))
    | Some parent -> Queue.enqueue parent.children record
  ;;

  let backtrace frame =
    let rec loop (frame : Frame.t) acc =
      let acc = frame.message :: acc in
      match frame.parent with
      | None -> acc
      | Some parent -> loop parent acc
    in
    List.rev (loop frame [])
  ;;
end

let maybe_record_frame ?hide_if_less_than:local_hide_if_less_than (frame : Frame.t) ~stop
  =
  let took = Time_ns.diff stop frame.start in
  let hide_if_less_than =
    Option.value local_hide_if_less_than ~default:!hide_if_less_than
  in
  if Time_ns.Span.( >= ) took hide_if_less_than
  then Profile_context.record_profile frame ~stop
;;

let on_async_out_of_order =
  ref (fun sexp -> !output_profile (Sexp.to_string_hum (force sexp) ^ "\n"))
;;

let record_profile ?hide_if_less_than (frame : Frame.t) =
  if frame.pending_children <> 0
  then (
    (* Pull this out of the record eagerly so we don't have problems with the lazy
       expression being evaluated later, where there might be an intervening write to
       [frame.pending_children]. *)
    let pending_children = frame.pending_children in
    !on_async_out_of_order
      (lazy
        [%message
          "Nested [profile Async] exited out-of-order."
            ~message:(Message.force frame.message : Sexp.t)
            (pending_children : int)]));
  maybe_record_frame ?hide_if_less_than frame ~stop:(now ())
;;

module Sync_or_async = struct
  type _ t =
    | Sync : _ t
    | Async : _ Deferred.t t
  [@@deriving sexp_of]
end

let profile_context_key =
  Univ_map.Key.create ~name:"Nested_profile.Profile.Frame" [%sexp_of: Frame.t]
;;

let current_profile_context () = Async_kernel_scheduler.find_local profile_context_key

let with_profile_context frame ~f =
  Async_kernel_scheduler.with_local profile_context_key frame ~f
;;

let profile
      (type a)
      ?hide_if_less_than
      (sync_or_async : a Sync_or_async.t)
      (message : Sexp.t Lazy.t)
      (f : unit -> a)
  : a
  =
  if not (!profiling_is_allowed && !should_profile)
  then f ()
  else (
    let tag =
      with_profiling_disallowed (fun () ->
        try Option.bind !tag_frames_with ~f:(fun f -> f ()) with
        | exn ->
          let backtrace = Backtrace.Exn.most_recent () in
          Some
            [%message
              "[Profile.tag_frames_with] raised" (exn : exn) (backtrace : Backtrace.t)])
    in
    let message =
      match tag with
      | None -> message
      | Some tag -> lazy (List [ force message; tag ])
    in
    let parent = current_profile_context () in
    let frame = Frame.create ~message ~parent in
    let incr_pending_children =
      match parent with
      | None -> fun ~by:_ -> ()
      | Some parent ->
        fun ~by ->
          parent.pending_children <- parent.pending_children + by;
          parent.max_pending_children
          <- Int.max parent.max_pending_children parent.pending_children
    in
    incr_pending_children ~by:1;
    let f () = with_profile_context (Some frame) ~f in
    match sync_or_async with
    | Sync ->
      Exn.protect ~f ~finally:(fun () ->
        record_profile ?hide_if_less_than frame;
        incr_pending_children ~by:(-1))
    | Async ->
      Monitor.protect f ~finally:(fun () ->
        record_profile ?hide_if_less_than frame;
        incr_pending_children ~by:(-1);
        return ()))
;;

let backtrace () =
  let frame = current_profile_context () in
  match !should_profile with
  | false -> None
  | true ->
    Some
      (Option.value_map frame ~f:Profile_context.backtrace ~default:[]
       |> List.map ~f:Message.force)
;;

let disown f = with_profile_context None ~f

module Private = struct
  module Clock = Clock

  let clock = clock
  let on_async_out_of_order = on_async_out_of_order

  let record_frame ~start ~stop ~message =
    if !profiling_is_allowed && !should_profile
    then
      maybe_record_frame
        { message = Message.create message
        ; start
        ; children = Queue.create ()
        ; parent = current_profile_context ()
        ; pending_children = 0
        ; max_pending_children = 0
        }
        ~stop
  ;;
end
