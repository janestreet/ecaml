open! Core
open! Async

let%expect_test "test generating intermediate logs from .t files" =
  let intermediate_file = Filename_unix.temp_file "test-hello" ".profile" in
  let%bind () =
    Process.run_exn
      ~env:
        (`Extend
          [ "HGRCPATH", ""
          ; (* This is a hack and will need to be updated if this file is moved *)
            "ROOT", Filename_unix.realpath "../../.."
          ])
      ~prog:"./run-unified-tests"
      ~args:
        [ "./test-hello.t"
        ; "--profile"
        ; "/dev/null"
        ; "--profile-intermediate-file"
        ; intermediate_file
        ; "--with-hg"
        ; "/usr/bin/hg"
        ]
      ()
    >>| print_endline
  in
  [%expect
    {|
    .
    # Ran 1 tests, 0 skipped, 0 failed.
    |}];
  let%bind () =
    Process.run_exn
      ~prog:"bash"
      ~args:[ "-c"; [%string "cat %{intermediate_file} | sed 's/^[0-9]* /TIMESTAMP /'"] ]
      ()
    >>| print_endline
  in
  [%expect
    {|
    TIMESTAMP echo hello
    TIMESTAMP echo world
    TIMESTAMP END
    |}];
  return ()
;;

let%expect_test "test generating Nested_profiles from intermediate logs" =
  let tmpfile = Filename_unix.temp_file "test_render_profile" ".sexp" in
  Monitor.protect
    ~finally:(fun () -> Unix.unlink tmpfile)
    (fun () ->
      let%bind () =
        Process.run_exn
          ~prog:"../bin/render_unified_test_profile.exe"
          ~args:
            [ "-output-file"
            ; tmpfile
            ; "-test-name"
            ; "test-hg.t"
            ; "test_profile_intermediate_file.log"
            ]
          ()
        >>| print_endline
      in
      let%bind () = Reader.file_contents tmpfile >>| print_endline in
      [%expect
        {|
        (1_900_000us test-hg.t "2021-11-09 16:08:17.455591583Z" (
           ( 17%   330_000us start_test)
           (  0%     2_000us "cat >can_load_re2.py <<EOF")
           ( 46%   860_000us "hg --config extensions.can_load_re2=can_load_re2.py --version | grep ok")
           (  3%    59_000us "cat $hgrc > $IRON_HGRCPATH")
           ( 20%   380_000us "hg pull foo -e 'echo >&2 running ssh'")
           ( 10%   190_000us "hg debuginstall | grep 'Python version'")
           (  4%    75_000us "hg debuginstall | grep 'Python version'")))
        |}];
      return ())
;;
