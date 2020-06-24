open! Core_kernel
open! Async_kernel
open! Import0
module Current_buffer = Current_buffer0

let inhibit_message = Var.Wrap.("inhibit-message" <: bool)

let inhibit_messages (type a b) (sync_or_async : (a, b) Sync_or_async.t) (f : unit -> b) =
  (match sync_or_async with
   | Sync -> ()
   | Async ->
     Background.assert_foreground
       [%here]
       ~message:
         [%sexp "Echo_area.inhibit_messages called asynchronously in background job"]);
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

let wrap_message ?echo message ~f =
  let message = concat [ message; " ... " ] in
  message_s ?echo [%sexp (message : string)];
  match%map Monitor.try_with f with
  | Ok x ->
    message_s ?echo [%sexp (concat [ message; "done" ] : string)];
    x
  | Error exn ->
    message_s ?echo [%sexp (concat [ message; "raised" ] : string)];
    raise exn
;;

let clear =
  let message = Funcall.Wrap.("message" <: value @-> return nil) in
  fun () -> message Value.nil
;;
