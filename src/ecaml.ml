(* We export all the Ecaml modules before doing [open!]s, because we want to
   export Ecaml modules that shadow [Core_kernel] ones. *)

module Advice            = Advice
module Ansi_color        = Ansi_color
module Auto_mode_alist   = Auto_mode_alist
module Backup            = Backup
module Browse_url        = Browse_url
module Buffer            = Buffer
module Char_code         = Char_code
module Color             = Color
module Command           = Command
module Comment           = Comment
module Compilation       = Compilation
module Completing        = Completing
module Current_buffer    = Current_buffer
module Customization     = Customization
module Defun             = Defun
module Directory         = Directory
module Display           = Display
module Echo_area         = Echo_area
module Face              = Face
module Feature           = Feature
module File              = File
module Filename          = Filename
module Find_function     = Find_function
module Form              = Form
module Frame             = Frame
module Funcall           = Funcall
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
module Regexp            = Regexp
module Rx                = Rx
module Selected_window   = Selected_window
module Symbol            = Symbol
module Syntax_table      = Syntax_table
module System            = System
module Tabulated_list    = Tabulated_list
module Text              = Text
module Thing_at_point    = Thing_at_point
module Timer             = Timer
module User              = User
module Value             = Value
module Valueable         = Valueable
module Var               = Var
module Vector            = Vector
module Window            = Window
module Working_directory = Working_directory

open! Core_kernel
open! Import

module Q = struct
  include Q
  let inhibit_read_only                = "inhibit-read-only"                |> Symbol.intern
end

let defcustom           = Customization. defcustom
let define_derived_mode = Major_mode.    define_derived_mode
let defun               = Defun.         defun
let defun_nullary       = Defun.         defun_nullary
let defun_nullary_nil   = Defun.         defun_nullary_nil
let defvar              = Form.          defvar
let describe_function   = Describe.      function_
let inhibit_messages    = Echo_area.     inhibit_messages
let lambda              = Defun.         lambda
let lambda_nullary      = Defun.         lambda_nullary
let lambda_nullary_nil  = Defun.         lambda_nullary_nil
let message             = Echo_area.     message
let messagef            = Echo_area.     messagef
let message_s           = Echo_area.     message_s

let provide =
  Ecaml_callback.(register end_of_module_initialization) ~f:(fun () ->
    message_s [%message "Loaded Ecaml."]);
  Caml_embed.initialize;
  Import.initialize_module;
  Find_function.initialize ();
  User.initialize ();
  Value.initialize_module;
  Feature.provide
;;

let inhibit_read_only = Var.create Q.inhibit_read_only Value.Type.bool

let inhibit_read_only f = Current_buffer.set_value_temporarily inhibit_read_only true ~f

let () =
  let symbol = "ecaml-test-raise" |> Symbol.intern in
  defun [%here] Value.Type.unit symbol ~interactive:""
    (let open Defun.Let_syntax in
     let%map_open n = optional Q.number Value.Type.int
     in
     let n = Option.value n ~default:0 in
     (if n <= 0
      then raise_s [%message "foo" "bar" "baz"]
      else Value.Type.(Funcall.(symbol <: option int @-> return nil)) (Some (n - 1)));
     ());
  (* Replace [false] with [true] to define a function for testing
     [Minibuffer.read_from]. *)
  if false
  then (
    defun_nullary_nil [%here]
      ("ecaml-test-minibuffer-y-or-n-with-timeout" |> Symbol.intern)
      ~interactive:""
      (fun () ->
         message_s
           [%message
             (Minibuffer.y_or_n_with_timeout ~prompt:"prompt"
                ~timeout:(Time_ns.Span.second, 13)
              : int Minibuffer.Y_or_n_with_timeout.t)]);
    defun_nullary_nil [%here] ("ecaml-test-minibuffer" |> Symbol.intern)
      ~interactive:""
      (fun () ->
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
         test () ~prompt:"test 4" ~history_list:("some-history-list" |> Symbol.intern)));
;;

let debug_embedded_caml_values () = Caml_embed.debug_sexp ()
