open! Core
open! Import

let with_buffer sync_or_async contents ~f =
  Current_buffer.set_temporarily_to_temp_buffer sync_or_async (fun () ->
    Point.insert_text (contents |> Text.of_utf8_bytes);
    f ())
;;

let with_buffer_and_point sync_or_async contents line_and_column ~f =
  with_buffer sync_or_async contents ~f:(fun () ->
    Point.goto_line_and_column line_and_column;
    f ())
;;

let utf8_full_block_U2588 = "\xE2\x96\x88"
let utf8_upper_left_U259B = "▛"
let utf8_lower_right_U259F = "▟"

let show_buffer ~block_out =
  let start, end_ = Point.min (), Point.max () in
  let all_contents =
    Current_buffer.save_restriction Sync (fun () ->
      Current_buffer.widen ();
      Current_buffer.contents () |> Text.to_utf8_bytes)
  in
  let start_marker = Marker.create () in
  let end_marker = Marker.create () in
  Marker.set_insertion_type end_marker After_inserted_text;
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert all_contents;
    Marker.set start_marker (Current_buffer.get ()) start;
    Marker.set end_marker (Current_buffer.get ()) end_;
    List.iter block_out ~f:(fun (position, replacement) ->
      let min = Point.min () in
      let max = Point.max () in
      let start = Position.clamp_exn position ~min ~max in
      let end_ = Position.clamp_exn (Position.add position 1) ~min ~max in
      Point.goto_char start;
      let contains_newline =
        Current_buffer.contents ~start ~end_ ()
        |> Text.to_utf8_bytes
        |> String.is_substring ~substring:"\n"
      in
      Current_buffer.delete_region ~start ~end_;
      Point.insert replacement;
      if contains_newline then Point.insert "\n");
    let start = Marker.position start_marker |> Option.value_exn in
    let end_ = Marker.position end_marker |> Option.value_exn in
    if Position.( <> ) start (Point.min ()) then message "<narrowed>";
    message (Current_buffer.contents ~start ~end_ () |> Text.to_utf8_bytes);
    if Position.( <> ) end_ (Point.max ()) then message "<narrowed>")
;;

let show_point () = show_buffer ~block_out:[ Point.get (), utf8_full_block_U2588 ]

let show_active_region () =
  match Current_buffer.active_region () with
  | None -> print_s [%message "No region is active."]
  | Some (start, end_) ->
    show_buffer ~block_out:[ start, utf8_upper_left_U259B; end_, utf8_lower_right_U259F ]
;;

(* The semantics of how to display overlay [before-string] and [after-string] properties
   are taken from the documentation of [overlay_strings] in buffer.c:

   {v
    /* Concatenate the strings associated with overlays that begin or end
      at POS, ignoring overlays that are specific to windows other than W.
      The strings are concatenated in the appropriate order: shorter
      overlays nest inside longer ones, and higher priority inside lower.
      Normally all of the after-strings come first, but zero-sized
      overlays have their after-strings ride along with the
      before-strings because it would look strange to print them
      inside-out.

      Returns the concatenated string's length, and return the pointer to
      that string via PSTR, if that variable is non-NULL.  The storage of
      the concatenated strings may be overwritten by subsequent calls.  */
   v} *)
let show_with_overlay_text () =
  Buffer.with_temp_buffer Sync (fun temp_buffer ->
    let write before text after =
      Current_buffer.set_temporarily Sync temp_buffer ~f:(fun () ->
        Point.insert before;
        Point.insert_text text;
        Point.insert after)
    in
    let write_text_property_if_present overlay property_name =
      match Overlay.get_property overlay property_name with
      | exception _ -> ()
      | s -> write "<overlay>" s "</overlay>"
    in
    Current_buffer.save_excursion Sync (fun () ->
      Point.goto_min ();
      let all_overlays = Overlay.in_ ~start:(Point.min ()) ~end_:(Point.max ()) in
      let all_endpoints =
        List.concat_map all_overlays ~f:(fun overlay ->
          [ Overlay.start overlay; Overlay.end_ overlay ])
        |> Position.Set.of_list
      in
      let rec loop () =
        if Position.( < ) (Point.get ()) (Point.max ())
        then (
          (* after-strings (for overlays ending here) are displayed before before-strings
             (for overlays beginning here), except that before-strings are displayed
             before after-strings for any empty overlays at this position. *)
          let overlays_starting_here = ref [] in
          let overlays_ending_here = ref [] in
          let empty_overlays = ref [] in
          List.iter all_overlays ~f:(fun o ->
            let start = Overlay.start o in
            let end_ = Overlay.end_ o in
            if Position.equal start (Point.get ())
            then
              if Position.equal end_ (Point.get ())
              then empty_overlays := o :: !empty_overlays
              else overlays_starting_here := o :: !overlays_starting_here
            else if Position.equal end_ (Point.get ())
            then overlays_ending_here := o :: !overlays_ending_here);
          (* Smaller overlays are printed before larger overlays *)
          let compare_overlay a b =
            Comparable.lift
              Int.compare
              ~f:(fun o -> Position.diff (Overlay.end_ o) (Overlay.start o))
              a
              b
          in
          (* These lists are sorted from "inside" to "outside". *)
          let overlays_starting_here =
            List.sort !overlays_starting_here ~compare:compare_overlay
          in
          let overlays_ending_here =
            List.sort !overlays_ending_here ~compare:compare_overlay
          in
          let empty_overlays =
            (* This sort does nothing right now, because compare_overlay only checks
               overlay length, but if we figure out how to sort by priority, it will be
               necessary to sort here. *)
            List.sort !empty_overlays ~compare:compare_overlay
          in
          (* Print "outer" strings first, for [after-string]s. *)
          List.iter (List.rev overlays_ending_here) ~f:(fun o ->
            write_text_property_if_present o Text.Property_name.after_string);
          List.iter empty_overlays ~f:(fun o ->
            write_text_property_if_present o Text.Property_name.before_string;
            write_text_property_if_present o Text.Property_name.after_string);
          List.iter overlays_starting_here ~f:(fun o ->
            write_text_property_if_present o Text.Property_name.before_string);
          (* Go to next overlay endpoint, or the end of the invisibility overlay if one
             starts here. If there is no invisibility overlay, write the buffer contents
             that we just skipped. *)
          let () =
            match
              List.filter_map overlays_starting_here ~f:(fun o ->
                match Overlay.get_property o Text.Property_name.invisible with
                | exception _ -> None
                | invisible ->
                  if Value.is_not_nil invisible then Some (Overlay.end_ o) else None)
              |> List.max_elt ~compare:Position.compare
            with
            | Some end_of_invisibility
              when (* Avoid infinite loop *)
                   Position.( > ) end_of_invisibility (Point.get ()) ->
              write
                "<invisible>"
                (Current_buffer.contents
                   ~start:(Point.get ())
                   ~end_:end_of_invisibility
                   ~text_properties:true
                   ())
                "</invisible>";
              Point.goto_char end_of_invisibility
            | Some _ | None ->
              let next_overlay_endpoint_or_end_of_buffer =
                match
                  Set.to_sequence
                    all_endpoints
                    ~greater_or_equal_to:(Position.add (Point.get ()) 1)
                  |> Sequence.hd
                with
                | Some next_overlay_endpoint -> next_overlay_endpoint
                | None -> Point.max ()
              in
              write
                ""
                (Current_buffer.contents
                   ~start:(Point.get ())
                   ~end_:next_overlay_endpoint_or_end_of_buffer
                   ~text_properties:true
                   ())
                "";
              Point.goto_char next_overlay_endpoint_or_end_of_buffer
          in
          loop ())
      in
      loop ();
      Current_buffer.set_temporarily Sync temp_buffer ~f:(fun () ->
        show_buffer ~block_out:[])))
;;

module Sample_input = struct
  let table1 =
    {|
┌──────────────────────────────────────┬─────┬──────┬────────┬───────────┐
│ feature                              │ CRs │ XCRs │ review │ next step │
├──────────────────────────────────────┼─────┼──────┼────────┼───────────┤
│ jane                                 │     │      │        │           │
│   plugd                              │     │      │        │ fix build │
│     rewrite-flags                    │   1 │    1 │      9 │           │
└──────────────────────────────────────┴─────┴──────┴────────┴───────────┘
|}
  ;;

  let table2 =
    {|
Features you own:
┌──────────────────────────┬─────┬──────┬───────┬───────────────────────┐
│ feature                  │ CRs │ XCRs │ #left │ next step             │
├──────────────────────────┼─────┼──────┼───────┼───────────────────────┤
│ jane                     │     │      │       │                       │
│   plugd                  │     │      │       │ fix build             │
│     clean-up-obligations │     │      │     3 │ review                │
│     commander            │     │      │       │ rebase, release       │
│   versioned-types        │     │      │     1 │ review                │
│     pipe-rpc             │     │      │     1 │ rebase, enable-review │
└──────────────────────────┴─────┴──────┴───────┴───────────────────────┘
|}
  ;;
end
