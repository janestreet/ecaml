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
