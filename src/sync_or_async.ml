open! Core_kernel
open! Import0
open! Async_kernel

type ('a, 'b) t =
  | Sync : ('a, 'a) t
  | Async : ('a, 'a Deferred.t) t
[@@deriving sexp_of]

let async_protect ~f ~finally =
  Monitor.protect f ~finally:(fun () ->
    finally ();
    return ())
;;

let return (type a b) (t : (a, b) t) (a : a) : b =
  match t with
  | Sync -> a
  | Async -> return a
;;

let protect
      (type a b)
      ?(allow_in_background = false)
      here
      (t : (a, b) t)
      ~(f : unit -> b)
      ~finally
  : b
  =
  match t with
  | Sync -> Exn.protect ~f ~finally
  | Async ->
    async_protect
      ~f:(fun () ->
        if not allow_in_background then Background.assert_foreground here;
        f ())
      ~finally
;;
