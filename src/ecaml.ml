(* We export all the Ecaml modules before doing [open!]s, because we want to
   export Ecaml modules that shadow [Core_kernel] ones. *)

module Advice            = Advice
module Ansi_color        = Ansi_color
module Auto_mode_alist   = Auto_mode_alist
module Backup            = Backup
module Buffer            = Buffer
module Char_code         = Char_code
module Color             = Color
module Command           = Command
module Comment           = Comment
module Compilation       = Compilation
module Current_buffer    = Current_buffer
module Customization     = Customization
module Directory         = Directory
module Echo_area         = Echo_area
module Face              = Face
module Feature           = Feature
module File              = File
module Filename          = Filename
module Find_function     = Find_function
module Form              = Form
module Frame             = Frame
module Function          = Function
module Grep              = Grep
module Hash_table        = Hash_table
module Hook              = Hook
module Input_event       = Input_event
module Key_sequence      = Key_sequence
module Keymap            = Keymap
module Load              = Load
module Load_history      = Load_history
module Major_mode        = Major_mode
module Marker            = Marker
module Minibuffer        = Minibuffer
module Minor_mode        = Minor_mode
module Obarray           = Obarray
module Point             = Point
module Position          = Position
module Process           = Process
module Q                 = Q
module Regexp            = Regexp
module Selected_window   = Selected_window
module Symbol            = Symbol
module Syntax_table      = Syntax_table
module System            = System
module Text              = Text
module Timer             = Timer
module User              = User
module Value             = Value
module Var               = Var
module Vector            = Vector
module Window            = Window
module Working_directory = Working_directory

open! Core_kernel
open! Import

let defadvice = Advice.defadvice

let defun = Function.defun

let defcustom = Customization.defcustom

let defvar = Form.defvar

let define_derived_mode = Major_mode.define_derived_mode

let inhibit_messages = Echo_area.inhibit_messages
let message          = Echo_area.message
let message_s        = Echo_area.message_s
let messagef         = Echo_area.messagef

let provide =
  Ecaml_callback.(register end_of_module_initialization) ~f:(fun () ->
    message_s [%message "Loaded Ecaml."]);
  Import.initialize_module;
  Find_function.initialize ();
  User.initialize ();
  Value.initialize_module;
  Feature.provide
;;

let inhibit_read_only = Var.create Q.inhibit_read_only Value.Type.bool

let inhibit_read_only f = Current_buffer.set_value_temporarily inhibit_read_only true ~f

let () =
  defun [%here] ("ecaml-test-raise" |> Symbol.intern)
    ~args:[]
    ~interactive:""
    (fun _ ->
       raise_s [%message "foo" "bar"]);
  (* Replace [false] with [true] to define a function for testing
     [Minibuffer.read_from]. *)
  if false
  then (
    defun [%here] ("ecaml-test-minibuffer-y-or-n-with-timeout" |> Symbol.intern)
      ~args:[]
      ~interactive:""
      (fun _ ->
         message_s
           [%message
             (Minibuffer.y_or_n_with_timeout ~prompt:"prompt"
                ~timeout:(Time_ns.Span.second, 13)
              : int Minibuffer.Y_or_n_with_timeout.t)];
         Value.nil);
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
