open! Core
open! Import

module Q = struct
  let ansi_color = "ansi-color" |> Symbol.intern
end

let ansi_color_apply_on_region =
  Funcall.Wrap.(
    "ansi-color-apply-on-region" <: Position.t @-> Position.t @-> return ignored)
;;

let apply_on_region ~start ~end_ =
  Feature.require Q.ansi_color;
  let ansi_color_context_region =
    Buffer_local.wrap_existing
      (Symbol.intern "ansi-color-context-region")
      Value.Type.value
  in
  Current_buffer.set_buffer_local_temporarily
    Sync
    ansi_color_context_region
    Value.nil
    ~f:(fun () -> ansi_color_apply_on_region start end_)
;;

let color_current_buffer () =
  let buffer_was_modified = Current_buffer.is_modified () in
  Current_buffer.(set_buffer_local read_only) false;
  Current_buffer.set_undo_enabled false;
  apply_on_region ~start:(Point.min ()) ~end_:(Point.max ());
  if not buffer_was_modified then Current_buffer.set_modified false;
  Current_buffer.(set_buffer_local read_only) true;
  Point.goto_min ()
;;
