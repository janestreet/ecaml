open! Core_kernel
open! Async_kernel
open! Import
open! Process

let%expect_test "[all_my_children], [name]" =
  let ts = all_emacs_children () in
  print_s [%message "" ~_:(ts : t list)];
  [%expect {| ("#<process Async scheduler>") |}];
  List.iter ts ~f:(fun t -> print_s [%message "" ~name:(name t : string)]);
  [%expect {| (name "Async scheduler") |}];
  return ()
;;

let sleep ?buffer () = create "/bin/sleep" [ "100" ] ~name:"sleeper" ?buffer ()

let show t =
  print_s
    [%message
      ""
        ~name:(name t : string)
        ~buffer:(buffer t : Buffer.t option)
        ~command:(command t : string list option)
        ~is_in_all_emacs_children:(List.mem (all_emacs_children ()) t ~equal : bool)
        ~pid_is_positive:
          (match pid t with
           | None -> None
           | Some pid -> Some (Pid.to_int pid >= 0)
                         : bool option)
        ~status:(status t : Status.t)
        ~exit_status:(exit_status t : Exit_status.t)]
;;

let%expect_test "[start], [buffer], [name], [command], [pid], [status], [exit_status], \
                 [kill]"
  =
  let t = sleep () in
  print_s [%sexp (t : t)];
  [%expect {|
    "#<process sleeper>" |}];
  show t;
  [%expect
    {|
    ((name sleeper)
     (buffer ())
     (command ((/bin/sleep 100)))
     (is_in_all_emacs_children true)
     (pid_is_positive (true))
     (status      Run)
     (exit_status Not_exited)) |}];
  kill t;
  show t;
  [%expect
    {|
    ((name sleeper)
     (buffer ())
     (command ((/bin/sleep 100)))
     (is_in_all_emacs_children false)
     (pid_is_positive (true))
     (status Signal)
     (exit_status (Fatal_signal 9))) |}];
  return ()
;;

let%expect_test "[exit_status]" =
  let test prog =
    let t = create prog [] ~name:"t" () in
    let%bind () =
      while_
        (fun () -> Process.is_alive t)
        ~do_:(fun () -> Timer.sleep_for (0.01 |> sec_ns))
    in
    print_s [%message (status t : Status.t) (exit_status t : Exit_status.t)];
    return ()
  in
  let%bind () = test "true" in
  [%expect {| (("status t" Exit) ("exit_status t" (Exited 0))) |}];
  let%bind () = test "false" in
  [%expect {| (("status t" Exit) ("exit_status t" (Exited 1))) |}];
  return ()
;;

let%expect_test "[extend_sentinel]" =
  let test prog =
    let t = create prog [] ~name:"t" () in
    let sentinels_ran = ref false in
    extend_sentinel [%here] t (Returns Value.Type.unit) ~sentinel:(fun ~event ->
      print_endline event);
    extend_sentinel [%here] t (Returns Value.Type.unit) ~sentinel:(fun ~event:_ ->
      print_s [%sexp "I'm another sentinel!"];
      sentinels_ran := true);
    let timeout_at = Time.(add (now ()) (Span.of_sec 1.)) in
    while_
      (fun () -> (not !sentinels_ran) && Time.(now () < timeout_at))
      ~do_:(fun () -> Timer.sleep_for (0.01 |> sec_ns))
  in
  let%bind () = test "true" in
  [%expect {|
    finished

    "I'm another sentinel!" |}];
  let%bind () = test "false" in
  [%expect {|
    exited abnormally with code 1

    "I'm another sentinel!" |}];
  return ()
;;

let%expect_test "[extend_sentinel] runs sentinel in the background" =
  let test (type a) (sync_or_async : (unit, a) Sync_or_async.t) =
    let t = create "true" [] ~name:"t" () in
    let sentinel_ran = ref false in
    extend_sentinel
      [%here]
      t
      (Returns.returns sync_or_async Value.Type.unit)
      ~sentinel:(fun ~event:_ ->
        sentinel_ran := true;
        print_s [%message "sentinel ran"];
        require [%here] (Background.am_running_in_background ());
        Sync_or_async.return sync_or_async ());
    while_
      (fun () -> not !sentinel_ran)
      ~do_:(fun () -> Timer.sleep_for (0.01 |> sec_ns))
  in
  let%bind () = test Sync in
  [%expect {| "sentinel ran" |}];
  let%bind () = test Async in
  [%expect {| "sentinel ran" |}];
  return ()
;;

let%expect_test "Async [extend_sentinel]" =
  let test prog =
    let t = create prog [] ~name:"t" () in
    let sentinels_ran = ref false in
    extend_sentinel [%here] t (Returns_deferred Value.Type.unit) ~sentinel:(fun ~event ->
      let%map () = Clock.after (sec 0.01) in
      print_endline event);
    extend_sentinel
      [%here]
      t
      (Returns_deferred Value.Type.unit)
      ~sentinel:(fun ~event:_ ->
        let%map () = Clock.after (sec 0.01) in
        print_s [%sexp "I'm another sentinel!"];
        sentinels_ran := true);
    let timeout_at = Time.(add (now ()) (Span.of_sec 1.)) in
    let rec loop () =
      if (not !sentinels_ran) && Time.(now () < timeout_at)
      then (
        let%bind () = Timer.sleep_for (0.01 |> sec_ns) in
        loop ())
      else return ()
    in
    loop ()
  in
  let%bind () = test "true" in
  [%expect {|
      finished

      "I'm another sentinel!" |}];
  let%bind () = test "false" in
  [%expect {|
      exited abnormally with code 1

      "I'm another sentinel!" |}];
  return ()
;;

let%expect_test "[exited]" =
  let t = create "true" [] ~name:"z" () in
  let exited = exited t in
  let rec loop () =
    if Deferred.is_determined exited
    then return ()
    else (
      let%bind () = Timer.sleep_for (0.01 |> sec_ns) in
      loop ())
  in
  let%bind () = loop () in
  print_s [%sexp (exited : Exited.t Deferred.t)];
  [%expect {|
      (Full (Exited 0)) |}];
  return ()
;;

let%expect_test "[buffer]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    let t = sleep () ~buffer:(Current_buffer.get ()) in
    print_s [%sexp (buffer t : Buffer.t option)];
    kill t);
  [%expect {| ("#<buffer *temp-buffer*>") |}];
  return ()
;;

let%expect_test "[find_by_name]" =
  let find () = find_by_name "sleeper" in
  let test () = print_s [%sexp (find () : t option)] in
  test ();
  [%expect {| () |}];
  ignore (sleep () : t);
  test ();
  [%expect {| ("#<process sleeper>") |}];
  Option.iter (find ()) ~f:kill;
  test ();
  [%expect {| () |}];
  return ()
;;

let%expect_test "[query_on_exit], [set_query_on_exit]" =
  let t = sleep () in
  let show () = print_s [%message "" ~query_on_exit:(query_on_exit t : bool)] in
  show ();
  [%expect {| (query_on_exit true) |}];
  set_query_on_exit t false;
  show ();
  [%expect {| (query_on_exit false) |}];
  kill t;
  return ()
;;

let print_current_buffer_contents () =
  print_string (Current_buffer.contents () |> Text.to_utf8_bytes)
;;

let test_call_result_exn
      ?input
      ?(output = Call.Output.Before_point_in_current_buffer)
      ?working_directory
      ()
      ~args
      ~prog
  =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    let result = call_result_exn prog args ?input ~output ?working_directory in
    print_s [%message (result : Call.Result.t)];
    print_endline "output:";
    print_current_buffer_contents ())
;;

let show_file_contents file =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    Point.insert_file_contents_exn file;
    print_current_buffer_contents ())
;;

let%expect_test "[call_result_exn] raise" =
  require_does_raise [%here] (fun () -> test_call_result_exn () ~prog:"/zzz" ~args:[]);
  [%expect
    {| (file-missing ("Searching for program" "No such file or directory" /zzz)) |}];
  return ()
;;

let%expect_test "[call_result_exn]" =
  test_call_result_exn () ~prog:"true" ~args:[];
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  test_call_result_exn () ~prog:"false" ~args:[];
  [%expect {|
    (result (Exit_status 1))
    output: |}];
  test_call_result_exn () ~prog:"echo" ~args:[ "foo"; "bar" ];
  [%expect {|
    (result (Exit_status 0))
    output:
    foo bar |}];
  return ()
;;

let%expect_test "[Call.Input.Dev_null]" =
  test_call_result_exn () ~prog:"cat" ~args:[] ~input:Dev_null;
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  return ()
;;

let%expect_test "[Call.Input.File]" =
  let file = Caml.Filename.temp_file "" "" in
  let%bind () = Selected_window.find_file file in
  Point.insert "foobar";
  let%bind () = Current_buffer.save () in
  Buffer.Blocking.kill (Current_buffer.get ());
  test_call_result_exn () ~prog:"cat" ~args:[ file ];
  [%expect {|
    (result (Exit_status 0))
    output:
    foobar |}];
  test_call_result_exn () ~prog:"cat" ~args:[] ~input:(File file);
  [%expect {|
    (result (Exit_status 0))
    output:
    foobar |}];
  Sys.remove file;
  return ()
;;

let%expect_test "[Call.Output.Dev_null]" =
  test_call_result_exn () ~prog:"echo" ~args:[ "foo" ] ~output:Dev_null;
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  return ()
;;

let%expect_test "[Call.Output.File]" =
  let file = Caml.Filename.temp_file "" "" in
  test_call_result_exn () ~prog:"echo" ~args:[ "foo" ] ~output:(Overwrite_file file);
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  show_file_contents file;
  [%expect {| foo |}];
  test_call_result_exn
    ()
    ~prog:"bash"
    ~args:[ "-c"; "echo 1>&2 another-foo" ]
    ~output:(Overwrite_file file);
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  show_file_contents file;
  [%expect {| another-foo |}];
  test_call_result_exn
    ()
    ~prog:"bash"
    ~args:[ "-c"; "echo foo; echo >&2 bar" ]
    ~output:(Overwrite_file file);
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  show_file_contents file;
  [%expect {|
    foo
    bar |}];
  Sys.remove file;
  return ()
;;

let%expect_test "[Call.Output.Split]" =
  Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
    test_call_result_exn
      ()
      ~prog:"echo"
      ~args:[ "foo" ]
      ~output:
        (Split { stderr = Dev_null; stdout = Before_point_in (Current_buffer.get ()) });
    [%expect {|
      (result (Exit_status 0))
      output: |}];
    print_current_buffer_contents ();
    [%expect {| foo |}]);
  test_call_result_exn
    ()
    ~prog:"echo"
    ~args:[ "foo" ]
    ~output:(Split { stderr = Dev_null; stdout = Before_point_in_current_buffer });
  [%expect {|
    (result (Exit_status 0))
    output:
    foo |}];
  test_call_result_exn
    ()
    ~prog:"echo"
    ~args:[ "foo" ]
    ~output:(Split { stderr = Dev_null; stdout = Dev_null });
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  let file = Caml.Filename.temp_file "" "" in
  test_call_result_exn
    ()
    ~prog:"bash"
    ~args:[ "-c"; "echo 1>&2 foo" ]
    ~output:(Split { stderr = Overwrite_file file; stdout = Dev_null });
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  show_file_contents file;
  [%expect {| foo |}];
  Sys.remove file;
  let file1 = Caml.Filename.temp_file "" "" in
  let file2 = Caml.Filename.temp_file "" "" in
  test_call_result_exn
    ()
    ~prog:"bash"
    ~args:[ "-c"; "echo foo; echo >&2 bar" ]
    ~output:(Split { stderr = Overwrite_file file1; stdout = Overwrite_file file2 });
  [%expect {|
    (result (Exit_status 0))
    output: |}];
  show_file_contents file1;
  [%expect {| bar |}];
  show_file_contents file2;
  [%expect {| foo |}];
  Sys.remove file1;
  Sys.remove file2;
  return ()
;;

let%expect_test "[call_exn]" =
  print_endline (call_exn "echo" [ "foo" ]);
  [%expect {| foo |}];
  return ()
;;

let%expect_test "[call_exn] raise" =
  show_raise (fun () -> call_exn "false" []);
  [%expect
    {|
    (raised (
      "[Process.call_exn] failed"
      (prog false)
      (args ())
      (result (Exit_status 1))
      (output ""))) |}];
  return ()
;;

let%expect_test "[shell_command_exn]" =
  print_endline (shell_command_exn "echo foo");
  [%expect {| foo |}];
  return ()
;;

let%expect_test "[shell_command_exn] raise" =
  show_raise (fun () -> shell_command_exn "echo -n foo; false");
  [%expect
    {|
    (raised (
      "[Process.call_exn] failed"
      (prog /bin/bash)
      (args   (-c          "echo -n foo; false"))
      (result (Exit_status 1))
      (output foo))) |}];
  return ()
;;

let%expect_test "[shell_command_exn ~working_directory]" =
  print_endline (shell_command_exn "pwd" ~working_directory:Root);
  [%expect {| / |}];
  print_endline (shell_command_exn "pwd" ~working_directory:(This "/bin"));
  [%expect {| /bin |}];
  require_equal
    [%here]
    (module String)
    (shell_command_exn "pwd" ~working_directory:Of_current_buffer)
    (Current_buffer.(get_buffer_local_exn directory)
     |> File.truename
     |> Filename.of_directory);
  return ()
;;

let input_region ~start ~end_ ~delete =
  Call.Region_input.Region
    { start = Position.of_int_exn start; end_ = Position.of_int_exn end_; delete }
;;

let test result =
  print_s [%message (result : Call.Result.t)];
  print_endline "output:";
  print_current_buffer_contents ();
  Current_buffer.erase ()
;;

let%expect_test "[call_region_exn]" =
  Point.insert "echo";
  test (call_region_exn "cat" [] ~output:Before_point_in_current_buffer);
  [%expect {|
    (result (Exit_status 0))
    output:
    echoecho |}];
  Point.insert "foooooooo";
  test (call_region_exn "sed" [ "s/o/i/g" ] ~output:Before_point_in_current_buffer);
  [%expect {|
    (result (Exit_status 0))
    output:
    foooooooofiiiiiiii |}];
  Point.insert "foobar";
  test
    (call_region_exn
       ~input:(input_region ~start:1 ~end_:5 ~delete:false)
       "sed"
       ~output:Before_point_in_current_buffer
       [ "s/o/i/g" ]);
  [%expect {|
    (result (Exit_status 0))
    output:
    foobarfiib |}];
  Point.insert "foobar";
  test
    (call_region_exn
       ~input:(input_region ~start:1 ~end_:5 ~delete:true)
       "sed"
       ~output:Before_point_in_current_buffer
       [ "s/o/i/g" ]);
  [%expect {|
    (result (Exit_status 0))
    output:
    arfiib |}];
  test
    (call_region_exn
       ~input:(String "footron")
       "sed"
       ~output:Before_point_in_current_buffer
       [ "s/o/i/g" ]);
  [%expect {|
    (result (Exit_status 0))
    output:
    fiitrin |}];
  return ()
;;

let%expect_test "[call_exn] with sexp error message" =
  List.iter Bool.all ~f:(fun verbose_exn ->
    show_raise (fun () ->
      call_exn ~verbose_exn "/bin/bash" [ "-c"; {|
echo '("foo bar" baz)'
exit 1
|} ]));
  [%expect
    {|
    (raised ("foo bar" baz))
    (raised (
      "[Process.call_exn] failed"
      (prog /bin/bash)
      (args (-c "\necho '(\"foo bar\" baz)'\nexit 1\n"))
      (result (Exit_status 1))
      (output ("foo bar"   baz)))) |}];
  return ()
;;

let%expect_test "[call_exn] with a multi-line error message" =
  List.iter Bool.all ~f:(fun verbose_exn ->
    show_raise (fun () ->
      call_exn
        ~verbose_exn
        "/bin/bash"
        [ "-c"; {|
echo line1
echo line2
echo line3
exit 1
|} ]));
  [%expect
    {|
    (raised (line1 line2 line3))
    (raised (
      "[Process.call_exn] failed"
      (prog /bin/bash)
      (args (-c "\necho line1\necho line2\necho line3\nexit 1\n"))
      (result (Exit_status 1))
      (output (line1 line2 line3)))) |}];
  return ()
;;

let%expect_test "[call_expect_no_output] with sexp error message" =
  List.iter Bool.all ~f:(fun verbose_exn ->
    show_raise (fun () ->
      call_exn ~verbose_exn "/bin/bash" [ "-c"; {|
echo '("foo bar" baz)'
exit 1
|} ]));
  [%expect
    {|
    (raised ("foo bar" baz))
    (raised (
      "[Process.call_exn] failed"
      (prog /bin/bash)
      (args (-c "\necho '(\"foo bar\" baz)'\nexit 1\n"))
      (result (Exit_status 1))
      (output ("foo bar"   baz)))) |}];
  return ()
;;

let%expect_test "[get_property] and [set_property]" =
  let t = Process.create "true" [] ~name:"z" () in
  let property = "foo" |> Symbol.intern in
  let show () = print_s [%sexp (get_property t property : Value.t option)] in
  show ();
  [%expect {| () |}];
  set_property t property (13 |> Value.of_int_exn);
  show ();
  [%expect {| (13) |}];
  return ()
;;
