open! Core
open! Async_kernel
open! Import

(* ['a] is a phantom type tracking whether the progress reporter was created with min/max
   values or not. *)
type 'a t =
  | Spinner : Value.t -> unit t
  | Range : Value.t -> int t
[@@deriving sexp_of]

let create_spinner_and_display =
  let make_progress_reporter =
    Funcall.Wrap.("make-progress-reporter" <: string @-> return value)
  in
  fun ~message -> Spinner (make_progress_reporter message)
;;

let create_with_range_and_display =
  let make_progress_reporter =
    Funcall.Wrap.("make-progress-reporter" <: string @-> int @-> int @-> return value)
  in
  fun ~min_incl ~max_incl ~message ->
    Range (make_progress_reporter message min_incl max_incl)
;;

let update_internal =
  let progress_reporter_force_update =
    Funcall.Wrap.(
      "progress-reporter-force-update"
      <: value @-> nil_or int @-> nil @-> nil_or string @-> return ignored)
  in
  let progress_reporter_update =
    Funcall.Wrap.(
      "progress-reporter-update"
      <: value @-> nil_or int @-> nil_or string @-> return ignored)
  in
  fun reporter progress suffix ->
    (* In tests, use [progress-reporter-force-update], which is deterministic because it
       does not check elapsed time. *)
    let f =
      match am_running_test with
      | false -> progress_reporter_update
      | true ->
        (* [progress-reporter-force-update] has an additional optional argument,
           [new-message], which allows one to change the message originally set at
           creation time. Rewrap this function to make the types match. *)
        fun reporter value suffix ->
        progress_reporter_force_update reporter value () suffix
    in
    f reporter progress suffix
;;

let update (type a) ?suffix (t : a t) (progress : a) =
  match t with
  | Spinner reporter ->
    let () = progress in
    update_internal reporter None suffix
  | Range reporter -> update_internal reporter (Some progress) suffix
;;

let progress_reporter_done =
  Funcall.Wrap.("progress-reporter-done" <: value @-> return ignored)
;;

let finish (type a) (t : a t) =
  match t with
  | Spinner value | Range value -> progress_reporter_done value
;;

module Deferred = struct
  module List = struct
    let iter ?suffix ~message list ~how ~f =
      let total = List.length list in
      let t = create_with_range_and_display ~min_incl:0 ~max_incl:total ~message in
      (* We have to count instead of just using [iteri], since with concurrent loops they
         might be completed out of order. *)
      let done_so_far = ref 0 in
      let%bind () =
        Deferred.List.iter list ~how ~f:(fun elt ->
          let%bind () = f elt in
          let suffix = Option.map suffix ~f:(fun f -> f elt) in
          incr done_so_far;
          update ?suffix t !done_so_far;
          return ())
      in
      finish t;
      return ()
    ;;
  end
end
