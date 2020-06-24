(* We export all the Ecaml modules before doing [open!]s, because we want to
   export Ecaml modules that shadow [Core_kernel] ones. *)

module Abbrev = Abbrev
module Advice = Advice
module Ansi_color = Ansi_color
module Async_ecaml = Async_ecaml
module Auto_mode_alist = Auto_mode_alist
module Background = Background
module Backup = Backup
module Bookmark = Bookmark
module Browse_url = Browse_url
module Buffer = Buffer
module Buffer_local = Buffer_local
module Caml_embed = Ecaml_value.Caml_embed
module Char_code = Char_code
module Clipboard = Clipboard
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
module Ecaml_profile = Ecaml_profile
module Echo_area = Echo_area
module Ediff = Ediff
module Elisp_gc = Elisp_gc
module Elisp_time = Elisp_time
module Emacs_backtrace = Emacs_backtrace
module Emacs_version = Emacs_version
module Eval = Eval
module Evil = Evil
module Expect_test_config = Async_ecaml.Expect_test_config
module Face = Face
module Feature = Feature
module File = File
module Filename = Filename
module Find_function = Find_function
module Form = Ecaml_value.Form
module Frame = Frame
module Funcall = Ecaml_value.Funcall
module Function = Ecaml_value.Function
module Grep = Grep
module Hash_table = Hash_table
module Help = Help
module Hook = Hook
module Input_event = Input_event
module Key_sequence = Key_sequence
module Keymap = Keymap
module Kill_ring = Kill_ring
module Line_and_column = Line_and_column
module Load = Load
module Load_history = Load_history
module Major_mode = Major_mode
module Marker = Marker
module Minibuffer = Minibuffer
module Minor_mode = Minor_mode
module Mode_line = Mode_line
module Modified_tick = Modified_tick
module Obarray = Obarray
module Obsolete = Obsolete
module Ocaml_or_elisp_value = Ocaml_or_elisp_value
module Org_table = Org_table
module Overlay = Overlay
module Plist = Plist
module Point = Point
module Position = Position
module Print = Print
module Process = Process
module Regexp = Regexp
module Rx = Rx
module Selected_window = Selected_window
module Symbol = Symbol
module Sync_or_async = Sync_or_async
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
module Variable_watcher = Variable_watcher
module Vector = Vector
module Window = Window
module Working_directory = Working_directory
open! Core_kernel
open! Async_kernel
open! Import
module Q = Q
include Async_ecaml.Export
include Composition_infix

let concat = concat
let defalias = Defun.defalias
let defconst = Defconst.defconst
let defconst_i = Defconst.defconst_i
let defcustom = Customization.defcustom
let defgroup = Customization.Group.defgroup
let define_derived_mode = Major_mode.define_derived_mode
let define_minor_mode = Minor_mode.define_minor_mode
let defun = Defun.defun
let defun_nullary = Defun.defun_nullary
let defun_nullary_nil = Defun.defun_nullary_nil
let defvar = Defvar.defvar
let defvaralias = Defvar.defvaralias
let inhibit_messages = Echo_area.inhibit_messages
let lambda = Defun.lambda
let lambda_nullary = Defun.lambda_nullary
let lambda_nullary_nil = Defun.lambda_nullary_nil
let message = Echo_area.message
let messagef = Echo_area.messagef
let message_s = Echo_area.message_s
let message_text = Echo_area.message_text
let print_s = print_s
let raise_string = raise_string
let sec_ns = sec_ns
let wrap_message = Echo_area.wrap_message

module Returns = Defun.Returns

let provide = (Feature.provide [@warning "-3"])
let inhibit_read_only = Current_buffer.inhibit_read_only

let () =
  if not am_running_inline_test
  then
    let module Unix = Core.Unix in
    let should_reopen_stdin = ref true in
    Background.Clock.every [%here] Time.Span.second (fun () ->
      match Unix.fstat Unix.stdin with
      | _ -> ()
      | exception _ ->
        if !should_reopen_stdin
        then (
          let new_fd = Unix.openfile "/dev/null" ~mode:[ O_RDONLY ] ~perm:0o666 in
          should_reopen_stdin := Unix.File_descr.equal new_fd Unix.stdin;
          message_s
            ~echo:false
            [%message.omit_nil
              "stdin was closed"
                (should_reopen_stdin : bool ref)
                ~recent_keys:
                  (Input_event.recent_commands_and_keys ()
                   : Input_event.Command_or_key.t array)]))
;;

let () =
  defun_nullary_nil
    ("ecaml-close-stdin" |> Symbol.intern)
    [%here]
    ~docstring:
      {|
Close file descriptor zero, aka stdin.  For testing a bug in `call-process-region'.
|}
    ~interactive:No_arg
    (fun () -> Core.Unix.(close stdin))
;;

let () =
  let ecaml_test_raise_name = "ecaml-test-raise" in
  let ecaml_test_raise =
    Funcall.Wrap.(ecaml_test_raise_name <: nil_or int @-> return nil)
  in
  defun
    (ecaml_test_raise_name |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open n = optional "number" int in
     let n = Option.value n ~default:0 in
     if n <= 0
     then raise_s [%message "foo" "bar" "baz"]
     else ecaml_test_raise (Some (n - 1)));
  (* Replace [false] with [true] to define a function for testing
     [Minibuffer.read_from]. *)
  if false
  then (
    defun_nullary
      ("ecaml-test-minibuffer-y-or-n-with-timeout" |> Symbol.intern)
      [%here]
      ~interactive:No_arg
      (Returns_deferred Value.Type.unit)
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
      (Returns_deferred Value.Type.unit)
      (fun () ->
         let test
               ?default_value
               ?(history = Minibuffer.history)
               ?history_pos
               ?initial_contents
               ()
               ~prompt
           =
           let%bind result =
             Minibuffer.read_from
               ~prompt:(concat [ prompt; ": " ])
               ?initial_contents
               ?default_value
               ~history
               ?history_pos
               ()
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
             (Minibuffer.History.find_or_create
                ("some-history-list" |> Symbol.intern)
                [%here])))
;;

let () =
  defun_nullary_nil
    ("ecaml-show-recent-commands-and-keys" |> Symbol.intern)
    [%here]
    ~interactive:No_arg
    (fun () ->
       message_s
         [%sexp
           (Input_event.recent_commands_and_keys () : Input_event.Command_or_key.t array)])
;;

let debug_embedded_caml_values () = Caml_embed.debug_sexp ()

module Ref = struct
  include Ref

  let set_temporarily_async r a ~f =
    let old = !r in
    r := a;
    Monitor.protect f ~finally:(fun () ->
      r := old;
      return ())
  ;;
end
