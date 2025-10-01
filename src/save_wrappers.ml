open! Core
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

let save_sync caller save_function args f =
  let r = ref None in
  let f =
    Function.of_ocaml_func0 caller (fun () ->
      r := Some (f ());
      Value.nil)
  in
  Form.apply
    save_function
    (List.map args ~f:Form.of_value_exn
     @ [ Form.apply Q.funcall [ Form.quote (Function.to_value f) ] ])
  |> Form.Blocking.eval_i;
  match !r with
  | None -> assert false
  | Some a -> a
;;

let save_
  (type a b)
  caller
  (sync_or_async : (a, b) Sync_or_async.t)
  save_function
  args
  (f : unit -> b)
  : b
  =
  match sync_or_async with
  | Sync -> save_sync caller save_function args f
  | Async ->
    Background.assert_foreground
      ~message:
        [%sexp
          (sprintf
             "%s called asynchronously in background job"
             (Symbol.name save_function)
           : string)]
      ();
    Value.Private.run_outside_async (fun () ->
      save_sync caller save_function args (fun () -> Value.Private.block_on_async f))
;;

let save_current_buffer ~(here : [%call_pos]) sync_or_async f =
  save_ here sync_or_async Q.save_current_buffer [] f
;;

let save_excursion ~(here : [%call_pos]) sync_or_async f =
  save_ here sync_or_async Q.save_excursion [] f
;;

let save_mark_and_excursion ~(here : [%call_pos]) sync_or_async f =
  save_ here sync_or_async Q.save_mark_and_excursion [] f
;;

let save_match_data ~(here : [%call_pos]) sync_or_async f =
  save_ here sync_or_async Q.save_match_data [] f
;;

let save_restriction ~(here : [%call_pos]) sync_or_async f =
  save_ here sync_or_async Q.save_restriction [] f
;;

let save_window_excursion ~(here : [%call_pos]) sync_or_async f =
  save_ here sync_or_async Q.save_window_excursion [] f
;;

let save_selected_window ~(here : [%call_pos]) sync_or_async f =
  save_ here sync_or_async Q.save_selected_window [] f
;;

let with_selected_frame ~(here : [%call_pos]) sync_or_async frame f =
  save_ here sync_or_async Q.with_selected_frame [ frame ] f
;;

let with_selected_window ~(here : [%call_pos]) sync_or_async window f =
  save_ here sync_or_async Q.with_selected_window [ window ] f
;;
