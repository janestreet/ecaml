open! Core_kernel

include Ecaml
include Expect_test_helpers_kernel

let concat = String.concat

let try_with = Or_error.try_with

(* [ignore_stderr] is useful for tests where Emacs outputs on stderr, which would cause
   jenga to view the test as failing.  [ignore_stderr] redirects stderr to [/dev/null],
   and returns a function to restore stderr. *)
let ignore_stderr () =
  let module Unix = Core.Unix in
  let saved_stderr = Unix.dup Unix.stderr in
  Unix.dup2
    ~src:(Unix.openfile "/dev/null" ~mode:[ O_WRONLY ])
    ~dst:Unix.stderr;
  stage (fun () ->
    Unix.dup2 ~src:saved_stderr ~dst:Unix.stderr;
    Unix.close saved_stderr)
;;

let background color : Text.Face_spec.t =
  [ Attributes [ T (Background, Color color) ]]
;;

let foreground color : Text.Face_spec.t =
  [ Attributes [ T (Foreground, Color color) ]]
;;

let background_blue = background Color.blue
let background_red  = background Color.red

let foreground_red  = foreground Color.red
let foreground_blue = foreground Color.blue

let show_last_match ?subexp () =
  let module Last_match = Regexp.Last_match in
  print_s [%message
    ""
      ~text:  (try_with (fun () -> Last_match.text_exn  ?subexp ()) : Text.t Or_error.t)
      ~start: (try_with (fun () -> Last_match.start_exn ?subexp ()) : int Or_error.t)
      ~end_:  (try_with (fun () -> Last_match.end_exn   ?subexp ()) : int Or_error.t)]
;;

let print_s ?(templatize_current_directory = false) sexp =
  if not templatize_current_directory
  then print_s sexp
  else (
    print_endline (
      sexp
      |> Sexp_pretty.sexp_to_string
      |> (fun string ->
        String.Search_pattern.replace_all
          (String.Search_pattern.create
             (File.truename Current_buffer.(value_exn directory)))
          ~in_:string
          ~with_:"<current-directory>/")))
;;

let touch filename =
  ignore (Process.call_exn "touch" [ filename ] ~working_directory:Of_current_buffer
          : string);
;;

let int_var name = Var.create (Symbol.create ~name) Value.Type.int

let show_current_buffer_local_variables () =
  print_s [%sexp (Buffer.buffer_local_variables (Current_buffer.get ())
                  |> List.sort ~cmp:(fun (s1, _) (s2, _) -> Symbol.compare_name s1 s2)
                  : (Symbol.t * _ option) list)];
;;

let with_input string f =
  let module Out_channel = Core.Out_channel in
  let module Unix = Core.Unix in
  let r, w = Unix.pipe () in
  let () = Unix.set_nonblock w in
  let out = Unix.out_channel_of_descr w in
  let stdin_to_restore = Unix.dup Unix.stdin in
  (try
     Out_channel.output_string out string;
     Out_channel.close out;
   with Sys_blocked_io ->
     raise_s [%message
       "[with_input] doesn't support strings this long"
         ~_:(String.length string : int)]);
  Unix.dup2 ~src:r ~dst:Unix.stdin;
  Unix.close r;
  protect ~f ~finally:(fun () ->
    Unix.dup2 ~src:stdin_to_restore ~dst:Unix.stdin)
;;

let%expect_test "[with_input] with too long string" =
  show_raise (fun () ->
    with_input (Bytes.create 100_000) (fun () -> assert false));
  [%expect {|
    (raised ("[with_input] doesn't support strings this long" 100_000)) |}];
;;

let () = Current_buffer.set_value Backup.make_backup_files false
