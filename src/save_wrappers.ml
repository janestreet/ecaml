open! Core_kernel
open! Import

module Q = struct
  include Q

  let save_current_buffer = "save-current-buffer" |> Symbol.intern
  let save_excursion = "save-excursion" |> Symbol.intern
  let save_mark_and_excursion = "save-mark-and-excursion" |> Symbol.intern
  let save_match_data = "save-match-data" |> Symbol.intern
  let save_restriction = "save-restriction" |> Symbol.intern
  let save_selected_window = "save-selected-window" |> Symbol.intern
  let save_window_excursion = "save-window-excursion" |> Symbol.intern
  let with_selected_frame = "with-selected-frame" |> Symbol.intern
  let with_selected_window = "with-selected-window" |> Symbol.intern
end

let save_sync save_function args f =
  let r = ref None in
  let f = Defun.lambda_nullary_nil [%here] (fun () -> r := Some (f ())) in
  ignore
    (Form.Blocking.eval
       (Form.list
          (List.concat
             [ [ save_function |> Form.symbol ]
             ; args |> List.map ~f:Form.of_value_exn
             ; [ Form.list
                   [ Q.funcall |> Form.symbol; f |> Function.to_value |> Form.quote ]
               ]
             ]))
     : Value.t);
  match !r with
  | None -> assert false
  | Some a -> a
;;

let save_
      (type a b)
      (sync_or_async : (a, b) Sync_or_async.t)
      save_function
      args
      (f : unit -> b)
  : b
  =
  match sync_or_async with
  | Sync -> save_sync save_function args f
  | Async ->
    Background.assert_foreground
      [%here]
      ~message:
        [%sexp
          (sprintf
             "%s called asynchronously in background job"
             (Symbol.name save_function)
           : string)];
    Value.Private.run_outside_async [%here] (fun () ->
      save_sync save_function args (fun () -> Value.Private.block_on_async [%here] f))
;;

let save_current_buffer sync_or_async f = save_ sync_or_async Q.save_current_buffer [] f
let save_excursion sync_or_async f = save_ sync_or_async Q.save_excursion [] f

let save_mark_and_excursion sync_or_async f =
  save_ sync_or_async Q.save_mark_and_excursion [] f
;;

let save_match_data sync_or_async f = save_ sync_or_async Q.save_match_data [] f
let save_restriction sync_or_async f = save_ sync_or_async Q.save_restriction [] f

let save_window_excursion sync_or_async f =
  save_ sync_or_async Q.save_window_excursion [] f
;;

let save_selected_window sync_or_async f =
  save_ sync_or_async Q.save_selected_window [] f
;;

let with_selected_frame sync_or_async frame f =
  save_ sync_or_async Q.with_selected_frame [ frame ] f
;;

let with_selected_window sync_or_async window f =
  save_ sync_or_async Q.with_selected_window [ window ] f
;;
