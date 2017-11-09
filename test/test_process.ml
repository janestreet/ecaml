open! Core_kernel
open! Import
open! Process

let%expect_test "[all_my_children], [name]" =
  let ts = all_emacs_children () in
  print_s [%message "" ~_:(ts : t list)];
  [%expect {|
    () |}];
  List.iter ts ~f:(fun t ->
    print_s [%message "" ~name:(name t : string)]);
  [%expect {| |}];
;;

let sleep ?buffer () =
  create ?buffer () ~name:"sleeper" ~prog:"/bin/sleep" ~args:[ "100" ]
;;

let%expect_test "[start], [buffer], [name], [command], [pid], [status], [kill]" =
  let t = sleep () in
  print_s [%sexp (t : t)];
  [%expect {|
    "#<process sleeper>" |}];
  let show () =
    print_s [%message
      ""
        ~name:(name t : string)
        ~buffer:(buffer t : Buffer.t option)
        ~command:(command t : string list option)
        ~is_in_all_emacs_children:(List.mem (all_emacs_children ()) t ~equal : bool)
        ~pid_is_positive:(
          match pid t with
          | None -> None
          | Some pid -> Some (Pid.to_int pid >= 0) : bool option)
        ~status:(status t : Symbol.t)] in
  show ();
  [%expect {|
    ((name sleeper)
     (buffer ())
     (command ((/bin/sleep 100)))
     (is_in_all_emacs_children true)
     (pid_is_positive (true))
     (status run)) |}];
  kill t;
  show ();
  [%expect {|
    ((name sleeper)
     (buffer ())
     (command ((/bin/sleep 100)))
     (is_in_all_emacs_children false)
     (pid_is_positive (true))
     (status signal)) |}];
;;

let%expect_test "[buffer]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let t = sleep () ~buffer:(Current_buffer.get ()) in
    print_s [%sexp (buffer t : Buffer.t option)];
    kill t);
  [%expect {|
    ("#<buffer *temp-buffer*>") |}]
;;

let%expect_test "[find_by_name]" =
  let find () = find_by_name "sleeper" in
  let test () = print_s [%sexp (find () : t option)] in
  test ();
  [%expect {|
    () |}];
  ignore (sleep () : t);
  test ();
  [%expect {|
    ("#<process sleeper>") |}];
  Option.iter (find ()) ~f:kill;
  test ();
  [%expect {|
    () |}];
;;

let%expect_test "[query_on_exit], [set_query_on_exit]" =
  let t = sleep () in
  let show () =
    print_s [%message
      ""
        ~query_on_exit:(query_on_exit t : bool)] in
  show ();
  [%expect {|
    (query_on_exit true) |}];
  set_query_on_exit t false;
  show ();
  [%expect {|
    (query_on_exit false) |}];
  kill t;
;;

let test_call_result_exn
      ?input
      ?(output = Call.Output.Before_point_in_current_buffer)
      ?working_directory
      () ~args ~prog =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    let result = call_result_exn prog args ?input ~output ?working_directory in
    print_s [%message
      ""
        (result : Call.Result.t)
        ~output:(Current_buffer.contents () : Text.t)]);
;;

let show_file_contents file =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Point.insert_file_contents_exn file;
    print_s [%sexp (Current_buffer.contents () : Text.t)]);
;;

let%expect_test "[call_result_exn] raise" =
  require_does_raise [%here] (fun () -> test_call_result_exn () ~prog:"/zzz" ~args:[]);
  [%expect {|
    (file-error ("Searching for program" "No such file or directory" /zzz)) |}];
;;

let%expect_test "[call_result_exn]" =
  test_call_result_exn () ~prog:"true" ~args:[];
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
  test_call_result_exn () ~prog:"false" ~args:[];
  [%expect {|
    ((result (Exit_status 1)) (output "")) |}];
  test_call_result_exn () ~prog:"echo" ~args:[ "foo"; "bar" ];
  [%expect {|
    ((result (Exit_status 0)) (output "foo bar\n")) |}];
;;

let%expect_test "[Call.Input.Dev_null]" =
  test_call_result_exn () ~prog:"cat" ~args:[] ~input:Dev_null;
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
;;

let%expect_test "[Call.Input.File]" =
  let file = Caml.Filename.temp_file "" "" in
  Selected_window.find_file file;
  Point.insert "foobar";
  Current_buffer.save ();
  Current_buffer.kill ();
  test_call_result_exn () ~prog:"cat" ~args:[file];
  [%expect {|
    ((result (Exit_status 0)) (output foobar)) |}];
  test_call_result_exn () ~prog:"cat" ~args:[] ~input:(File file);
  [%expect {|
    ((result (Exit_status 0)) (output foobar)) |}];
  Sys.remove file;
;;

let%expect_test "[Call.Output.Dev_null]" =
  test_call_result_exn () ~prog:"echo" ~args:[ "foo" ] ~output:Dev_null;
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
;;

let%expect_test "[Call.Output.File]" =
  let file = Caml.Filename.temp_file "" "" in
  test_call_result_exn () ~prog:"echo" ~args:[ "foo" ] ~output:(Overwrite_file file);
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
  show_file_contents file;
  [%expect {|
    "foo\n" |}];
  test_call_result_exn () ~prog:"bash" ~args:[ "-c"; "echo 1>&2 another-foo" ]
    ~output:(Overwrite_file file);
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
  show_file_contents file;
  [%expect {|
    "another-foo\n" |}];
  test_call_result_exn () ~prog:"bash" ~args:[ "-c"; "echo foo; echo >&2 bar" ]
    ~output:(Overwrite_file file);
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
  show_file_contents file;
  [%expect {|
    "foo\nbar\n" |}];
  Sys.remove file;
;;

let%expect_test "[Call.Output.Split]" =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    test_call_result_exn () ~prog:"echo" ~args:[ "foo" ]
      ~output:(Split { stderr = Dev_null
                     ; stdout = Before_point_in (Current_buffer.get ()) });
    [%expect {|
      ((result (Exit_status 0)) (output "")) |}];
    print_s [%sexp (Current_buffer.contents () : Text.t)];
    [%expect {|
      "foo\n" |}]);
  test_call_result_exn () ~prog:"echo" ~args:[ "foo" ]
    ~output:(Split { stderr = Dev_null; stdout = Before_point_in_current_buffer });
  [%expect {|
    ((result (Exit_status 0)) (output "foo\n")) |}];
  test_call_result_exn () ~prog:"echo" ~args:[ "foo" ]
    ~output:(Split { stderr = Dev_null; stdout = Dev_null });
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
  let file = Caml.Filename.temp_file "" "" in
  test_call_result_exn () ~prog:"bash" ~args:[ "-c"; "echo 1>&2 foo" ]
    ~output:(Split { stderr = Overwrite_file file; stdout = Dev_null });
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
  show_file_contents file;
  [%expect {|
    "foo\n" |}];
  Sys.remove file;
  let file1 = Caml.Filename.temp_file "" "" in
  let file2 = Caml.Filename.temp_file "" "" in
  test_call_result_exn () ~prog:"bash" ~args:[ "-c"; "echo foo; echo >&2 bar" ]
    ~output:(Split { stderr = Overwrite_file file1; stdout = Overwrite_file file2 });
  [%expect {|
    ((result (Exit_status 0)) (output "")) |}];
  show_file_contents file1;
  [%expect {|
    "bar\n" |}];
  show_file_contents file2;
  [%expect {|
    "foo\n" |}];
  Sys.remove file1;
  Sys.remove file2;
;;

let%expect_test "[call_exn]" =
  print_endline (call_exn "echo" [ "foo" ]);
  [%expect {|
    foo |}];
;;

let%expect_test "[call_exn] raise" =
  show_raise (fun () -> call_exn "false" []);
  [%expect {|
    (raised (
      "[Process.call_exn] failed"
      (prog false)
      (args ())
      (result (Exit_status 1))
      (output ""))) |}];
;;

let%expect_test "[shell_command_exn]" =
  print_endline (shell_command_exn "echo foo");
  [%expect {|
    foo |}];
;;

let%expect_test "[shell_command_exn] raise" =
  show_raise (fun () -> shell_command_exn "echo -n foo; false");
  [%expect {|
    (raised (
      "[Process.call_exn] failed"
      (prog /bin/bash)
      (args   (-c          "echo -n foo; false"))
      (result (Exit_status 1))
      (output foo))) |}];
;;

let%expect_test "[shell_command_exn ~working_directory]" =
  print_endline (shell_command_exn "pwd" ~working_directory:Root);
  [%expect {|
    / |}];
  print_endline (shell_command_exn "pwd" ~working_directory:(This "/bin"));
  [%expect {|
    /bin |}];
  require_equal [%here] (module String)
    (shell_command_exn "pwd" ~working_directory:Of_current_buffer)
    (Current_buffer.(value_exn directory) |> File.truename |> Filename.of_directory);
;;
