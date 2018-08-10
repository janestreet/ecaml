open Core
open Async

module Ping_pong = struct
  module Query = Unit
  module Response = Unit

  let rpc =
    Rpc.Rpc.create
      ~name:"ping-pong"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;

  module Server = struct
    let impl = Rpc.Rpc.implement rpc (fun _ _ -> Deferred.unit)

    let serve () =
      let implementations =
        Rpc.Implementations.create_exn ~implementations:[ impl ] ~on_unknown_rpc:`Raise
      in
      Rpc.Connection.serve
        ~implementations
        ~initial_connection_state:(fun _ _ -> ())
        ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
        ()
    ;;
  end

  module Client = struct
    let ping connection = Rpc.Rpc.dispatch_exn rpc connection ()

    let pings where_to_connect n =
      Rpc.Connection.with_client where_to_connect (fun conn ->
        let rec loop n =
          if n <= 0
          then Deferred.unit
          else (
            let%bind () = ping conn in
            loop (n - 1))
        in
        loop n)
      >>| Result.ok_exn
    ;;
  end
end

let benchmark_small_pings () =
  let n = 100 in
  let%bind server = Ping_pong.Server.serve () in
  let port = Tcp.Server.listening_on server in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let start = Time.now () in
  let%bind () = Ping_pong.Client.pings where_to_connect n in
  let diff = Time.diff (Time.now ()) start in
  let%map () = Tcp.Server.close server in
  [%sexp (Time.Span.(diff / Float.of_int n) : Time.Span.t)]
;;

module Throughput = struct
  module Query = Int
  module Response = String

  let rpc =
    Rpc.Rpc.create
      ~name:"throughput"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;

  module Server = struct
    let impl = Rpc.Rpc.implement rpc (fun _ n -> return (String.make n 'z'))

    let serve () =
      let implementations =
        Rpc.Implementations.create_exn ~implementations:[ impl ] ~on_unknown_rpc:`Raise
      in
      Rpc.Connection.serve
        ~implementations
        ~initial_connection_state:(fun _ _ -> ())
        ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
        ()
    ;;
  end

  module Client = struct
    let measure where_to_connect n =
      Rpc.Connection.with_client where_to_connect (fun conn ->
        Rpc.Rpc.dispatch_exn rpc conn n)
      >>| Result.ok_exn
      >>| ignore
    ;;
  end
end

let benchmark_throughput () =
  let n = 100_000_000 in
  let%bind server = Throughput.Server.serve () in
  let port = Tcp.Server.listening_on server in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let start = Time.now () in
  let%bind () = Throughput.Client.measure where_to_connect n in
  let diff = Time.diff (Time.now ()) start in
  let%map () = Tcp.Server.close server in
  let seconds = Time.Span.to_sec diff in
  let bytes_per_second = Float.iround_nearest_exn (Float.of_int n /. seconds) in
  [%sexp (bytes_per_second : int)]
;;
