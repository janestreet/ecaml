open! Core
open! Async_kernel
open! Import0
module Current_buffer = Current_buffer0

let inhibit_message = Var.Wrap.("inhibit-message" <: bool)

let inhibit_messages (type a b) (sync_or_async : (a, b) Sync_or_async.t) (f : unit -> b) =
  (match sync_or_async with
   | Sync -> ()
   | Async ->
     Background.assert_foreground
       ~message:
         [%sexp "Echo_area.inhibit_messages called asynchronously in background job"]
       ());
  Current_buffer.set_value_temporarily sync_or_async inhibit_message true ~f
;;

let maybe_echo ~echo f =
  if Option.value echo ~default:true then f () else inhibit_messages Sync f
;;

let message ?echo s = maybe_echo ~echo (fun () -> Value.message s)
let messagef ?echo fmt = ksprintf (message ?echo) fmt
let message_s ?echo s = maybe_echo ~echo (fun () -> Value.message_s s)

let message_text ?echo text =
  maybe_echo ~echo (fun () -> Value.Private.message_t (Text.to_value text))
;;

let wrap_message
  (type a b)
  ?allow_in_background
  ?echo
  ?(show_in_tests = true)
  ~(here : [%call_pos])
  (sync_or_async : (a, b) Sync_or_async.t)
  msg
  ~(f : unit -> b)
  =
  match am_running_test, show_in_tests with
  | true, false -> f ()
  | false, _ | _, true ->
    let msg = msg ^ " ... " in
    let returned_normally = ref false in
    message ?echo msg;
    let (f : unit -> b) =
      match sync_or_async with
      | Sync ->
        fun () ->
          let result = f () in
          returned_normally := true;
          result
      | Async ->
        fun () ->
          let%map result = f () in
          returned_normally := true;
          result
    in
    Sync_or_async.protect ?allow_in_background ~here sync_or_async ~f ~finally:(fun () ->
      match !returned_normally with
      | true -> message ?echo [%string "%{msg}done"]
      | false -> message ?echo [%string "%{msg}raised"])
;;

let clear =
  let message = Funcall.Wrap.("message" <: value @-> return nil) in
  fun () -> message Value.nil
;;
