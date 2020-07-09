open! Core_kernel
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

let show_buffer ~block_out =
  let contents = Current_buffer.contents () |> Text.to_utf8_bytes in
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert contents;
    List.iter block_out ~f:(fun position ->
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
      Point.insert utf8_full_block_U2588;
      if contains_newline then Point.insert "\n");
    message (Current_buffer.contents () |> Text.to_utf8_bytes))
;;

let show_point () = show_buffer ~block_out:[ Point.get () ]

module Region = struct
  type t =
    { start : Line_and_column.t
    ; end_ : Line_and_column.t
    }
  [@@deriving sexp_of]
end

open Region

let with_buffer_and_active_region sync_or_async contents { start; end_ } ~f =
  with_buffer sync_or_async contents ~f:(fun () ->
    Current_buffer.set_mark (Current_buffer.position_of_line_and_column start);
    Point.goto_line_and_column end_;
    f ())
;;

let show_active_region () =
  match Current_buffer.active_region () with
  | None -> print_s [%message "No region is active."]
  | Some (start, end_) -> show_buffer ~block_out:[ start; end_ ]
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
