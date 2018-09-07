open! Core
open! Async

val benchmark_small_pings : unit -> Sexp.t Deferred.t
val benchmark_throughput : unit -> Sexp.t Deferred.t
