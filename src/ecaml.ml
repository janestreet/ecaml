(* We export all the Ecaml modules before doing [open!]s, because we want to
   export Ecaml modules that shadow [Core_kernel] ones. *)

module Advice = Advice
module Ansi_color = Ansi_color
module Async_ecaml = Async_ecaml
module Auto_mode_alist = Auto_mode_alist
module Backup = Backup
module Browse_url = Browse_url
module Buffer = Buffer
module Buffer_local = Buffer_local
module Char_code = Char_code
module Color = Color
module Command = Command
module Comment = Comment
module Compilation = Compilation
module Completing = Completing
module Current_buffer = Current_buffer
module Customization = Customization
module Debugger = Debugger
module Defconst = Defconst
module Defun = Defun
module Defvar = Defvar
module Directory = Directory
module Display = Display
module Display_property = Display_property
module Documentation = Documentation
module Echo_area = Echo_area
module Elisp_time = Elisp_time
module Face = Face
module Feature = Feature
module File = File
module Filename = Filename
module Find_function = Find_function
module Form = Ecaml_value.Form
module Frame = Frame
module Funcall = Funcall
module Function = Ecaml_value.Function
module Grep = Grep
module Hash_table = Hash_table
module Help = Help
module Hook = Hook
module Input_event = Input_event
module Key_sequence = Key_sequence
module Keymap = Keymap
module Load = Load
module Load_history = Load_history
module Major_mode = Major_mode
module Marker = Marker
module Minibuffer = Minibuffer
module Minor_mode = Minor_mode
module Modified_tick = Modified_tick
module Obarray = Obarray
module Obsolete = Obsolete
module Ocaml_or_elisp_value = Ocaml_or_elisp_value
module Org_table = Org_table
module Overlay = Overlay
module Plist = Plist
module Point = Point
module Position = Position
module Process = Process
module Regexp = Regexp
module Rx = Rx
module Selected_window = Selected_window
module Symbol = Symbol
module Syntax_table = Syntax_table
module System = System
module Tabulated_list = Tabulated_list
module Terminal = Terminal
module Text = Text
module Thing_at_point = Thing_at_point
module Timer = Timer
module User = User
module Value = Ecaml_value.Value
module Valueable = Ecaml_value.Valueable
module Var = Var
module Vector = Vector
module Window = Window
module Working_directory = Working_directory
open! Core_kernel
open! Async_kernel
open! Import

module Q = struct
  include Q

  let inhibit_read_only = "inhibit-read-only" |> Symbol.intern
end

let ( << ) = ( << )
and ( >> ) = ( >> )
and concat = concat
and defalias = Defun.defalias
and defconst = Defconst.defconst
and defconst_i = Defconst.defconst_i
and defcustom = Customization.defcustom
and define_derived_mode = Major_mode.define_derived_mode
and define_minor_mode = Minor_mode.define_minor_mode
and defun = Defun.defun
and defun_nullary = Defun.defun_nullary
and defun_nullary_nil = Defun.defun_nullary_nil
and defvar = Defvar.defvar
and defvaralias = Defvar.defvaralias
and inhibit_messages = Echo_area.inhibit_messages
and lambda = Defun.lambda
and lambda_nullary = Defun.lambda_nullary
and lambda_nullary_nil = Defun.lambda_nullary_nil
and message = Echo_area.message
and messagef = Echo_area.messagef
and message_s = Echo_area.message_s
and print_s = print_s
and raise_string = raise_string
and sec_ns = sec_ns
and wrap_message = Echo_area.wrap_message

let provide =
  Ecaml_callback.(register end_of_module_initialization)
    ~should_run_holding_async_lock:true
    ~f:(fun () -> message_s [%message "Loaded Ecaml."]);
  Async_ecaml.initialize ();
  Caml_embed.initialize;
  Import.initialize_module;
  Find_function.initialize ();
  User.initialize ();
  Value.initialize_module;
  (Feature.provide [@warning "-3"])
;;

let inhibit_read_only = Var.create Q.inhibit_read_only Value.Type.bool
let inhibit_read_only f = Current_buffer.set_value_temporarily inhibit_read_only true ~f

let () =
  let symbol = "ecaml-test-raise" |> Symbol.intern in
  defun
    symbol
    [%here]
    ~interactive:No_arg
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open n = optional Q.number int in
     let n = Option.value n ~default:0 in
     if n <= 0
     then raise_s [%message "foo" "bar" "baz"]
     else Funcall.(symbol <: option int @-> return nil) (Some (n - 1));
     ());
  (* Replace [false] with [true] to define a function for testing
     [Minibuffer.read_from]. *)
  if false
  then (
    defun_nullary
      ("ecaml-test-minibuffer-y-or-n-with-timeout" |> Symbol.intern)
      [%here]
      ~interactive:No_arg
      Returns_unit_deferred
      (fun () ->
         let%bind int =
           Minibuffer.y_or_n_with_timeout
             ~prompt:"prompt"
             ~timeout:(Time_ns.Span.second, 13)
         in
         message_s [%message (int : int Minibuffer.Y_or_n_with_timeout.t)];
         return ());
    defun_nullary
      ("ecaml-test-minibuffer" |> Symbol.intern)
      [%here]
      ~interactive:No_arg
      Returns_unit_deferred
      (fun () ->
         let test ?default_value ?history ?history_pos ?initial_contents () ~prompt =
           let%bind result =
             Minibuffer.read_from
               ()
               ?default_value
               ?history
               ?history_pos
               ?initial_contents
               ~prompt:(concat [ prompt; ": " ])
           in
           message (concat [ "result: "; result ]);
           return ()
         in
         let%bind () = test () ~prompt:"test 1" in
         let%bind () = test () ~prompt:"test 2" ~default_value:"some-default" in
         let%bind () = test () ~prompt:"test 3" ~initial_contents:"some-contents" in
         test
           ()
           ~prompt:"test 4"
           ~history:
             (Var.create ("some-history-list" |> Symbol.intern) Value.Type.(list string))))
;;

let debug_embedded_caml_values () = Caml_embed.debug_sexp ()
