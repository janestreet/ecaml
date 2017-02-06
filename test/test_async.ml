open! Core
open! Async
open! Import

let%expect_test "basic upon" =
  let%bind () =
    print_s [%sexp "Hello, world!"];
    return () in
  [%expect {| "Hello, world!" |}]
;;

let%expect_test "wait 1 ms" =
  let%bind () = after (sec 0.001) >>| (fun () -> printf "hi!") in
  [%expect {| hi! |}]
;;

let%expect_test "[after] loop" =
  let rec loop i =
    if i < 0
    then return ()
    else (
      print_s [%message (i : int)];
      let%bind () = after (sec 0.001) in
      loop (i - 1))
  in
  let%bind () = loop 3 in
  [%expect {|
    (i 3)
    (i 2)
    (i 1)
    (i 0) |}];
;;

let%expect_test "pipes" =
  let r, w = Pipe.create () in
  don't_wait_for (
    let%bind () = Pipe.write w "ping" in
    let%bind () = Pipe.write w "pong" in
    Pipe.close w;
    return ());
  let%bind () = Pipe.iter_without_pushback r ~f:(printf "%s\n") in
  let%bind () = [%expect {|
    ping
    pong |}] in
  let%bind () =
    ["foo"; "bar"]
    |> Sequence.of_list
    |> Pipe.of_sequence
    |> Pipe.iter_without_pushback ~f:(printf "%s\n")
  in
  [%expect {|
    foo
    bar |}]
;;

let%expect_test "async conditions" =
  let cond = Condition.create () in
  let continue = Ivar.create () in
  don't_wait_for (
    let%bind () = Condition.wait cond in
    printf "1";
    Ivar.fill continue ();
    return ());
  Condition.signal cond ();
  let%bind () = Ivar.read continue in
  [%expect {|
    1 |}]
;;

let%expect_test "Writer" =
  for i = 1 to 10 do
    Writer.writef (force Writer.stdout) "%d\n" i
  done;
  [%expect {|
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10 |}]
;;

let%expect_test "pipe chains" =
  let rec filter_primes r w =
    match%bind Pipe.read r with
    | `Eof -> Pipe.close w; Deferred.unit
    | `Ok p ->
      let%bind () = Pipe.write_if_open w p in
      let filtered = Pipe.filter r ~f:(fun n -> n % p <> 0) in
      filter_primes filtered w
  in
  let numbers = Sequence.range 2 50 |> Pipe.of_sequence in
  let r, w = Pipe.create () in
  don't_wait_for (filter_primes numbers w);
  let%bind () = Pipe.iter_without_pushback r ~f:(printf "%d\n") in
  [%expect {|
    2
    3
    5
    7
    11
    13
    17
    19
    23
    29
    31
    37
    41
    43
    47 |}]
;;
