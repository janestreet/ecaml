open! Core
open! Async

let () =
  Command.run
    (Command.async
       ~summary:"Run ecaml_async benchmarks with the normal async_unix scheduler"
       (let%map_open.Command () = return () in
        fun () ->
          let%bind time_per_ping =
            Ecaml_bench.Bench_async_ecaml.benchmark_small_pings ()
          in
          let%map bytes_per_second =
            Ecaml_bench.Bench_async_ecaml.benchmark_throughput ()
          in
          print_s [%message (time_per_ping : Sexp.t) (bytes_per_second : Sexp.t)]))
;;
