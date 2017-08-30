(* We export all the Ecaml modules before doing [open!]s, because we want to
   export Ecaml modules that shadow [Core_kernel] ones. *)

module Ansi_color      = Ansi_color
module Buffer          = Buffer
module Color           = Color
module Command         = Command
module Current_buffer  = Current_buffer
module Echo_area       = Echo_area
module Face            = Face
module Feature         = Feature
module File            = File
module Filename        = Filename
module Form            = Form
module Frame           = Frame
module Function        = Function
module Hash_table      = Hash_table
module Load            = Load
module Load_history    = Load_history
module Marker          = Marker
module Minibuffer      = Minibuffer
module Point           = Point
module Position        = Position
module Process         = Process
module Q               = Q
module Regexp          = Regexp
module Selected_window = Selected_window
module Symbol          = Symbol
module Text            = Text
module Value           = Value
module Vector          = Vector
module Window          = Window

open! Core_kernel
open! Import

let defun = Function.defun

let message   = Echo_area.message
let message_s = Echo_area.message_s
let messagef  = Echo_area.messagef

let provide =
  Ecaml_callback.(register end_of_module_initialization) ~f:(fun () ->
    message_s [%message "Loaded Ecaml."]);
  Import.initialize_module;
  Value.initialize_module;
  Feature.provide
;;

let inhibit_read_only f =
  let old = Symbol.value_exn Q.inhibit_read_only in
  if old |> Value.to_bool
  then f ()
  else (
    let set b = Symbol.set_value Q.inhibit_read_only (b |> Value.of_bool) in
    set true;
    Exn.protect ~f ~finally:(fun () -> set false))
;;

let () =
  (* Replace [false] with [true] to define a function for testing
     [Minibuffer.read_from]. *)
  if false
  then (
    defun [%here] ("ecaml-test-minibuffer" |> Symbol.intern)
      ~args:[]
      ~interactive:""
      (fun _ ->
         let test ?default_value ?history_list ?history_list_pos ?initial_contents ()
               ~prompt =
           let result =
             Minibuffer.read_from ()
               ?default_value ?history_list ?history_list_pos ?initial_contents
               ~prompt:(concat [ prompt; ": " ])
           in
           message (concat [ "result: "; result ])
         in
         test () ~prompt:"test 1";
         test () ~prompt:"test 2" ~default_value:"some-default";
         test () ~prompt:"test 3" ~initial_contents:"some-contents";
         test () ~prompt:"test 4" ~history_list:("some-history-list" |> Symbol.intern);
         Value.nil));
;;
