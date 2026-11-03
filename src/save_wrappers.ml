open! Core
open! Import

module Q = struct
  include Q

  let save_excursion = "save-excursion" |> Symbol.intern
  let save_match_data = "save-match-data" |> Symbol.intern
  let save_restriction = "save-restriction" |> Symbol.intern
  let save_selected_window = "save-selected-window" |> Symbol.intern
  let save_window_excursion = "save-window-excursion" |> Symbol.intern
  let with_selected_frame = "with-selected-frame" |> Symbol.intern
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

let save_excursion ~(here : [%call_pos]) f = save_sync here Q.save_excursion [] f
let save_match_data ~(here : [%call_pos]) f = save_sync here Q.save_match_data [] f
let save_restriction ~(here : [%call_pos]) f = save_sync here Q.save_restriction [] f

let save_window_excursion ~(here : [%call_pos]) f =
  save_sync here Q.save_window_excursion [] f
;;

let save_selected_window ~(here : [%call_pos]) f =
  save_sync here Q.save_selected_window [] f
;;

let with_selected_frame ~(here : [%call_pos]) frame f =
  save_sync here Q.with_selected_frame [ frame ] f
;;
