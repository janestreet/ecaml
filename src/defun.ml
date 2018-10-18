open! Core_kernel
open! Import
include Defun0

let defun_blocking_async
      ?define_keys
      ?docstring
      ?interactive
      ?obsoletes
      ?(timeout = Some (Time.Span.of_sec 1.))
      here
      symbol
      t
  =
  defun_internal
    ?define_keys
    ?docstring
    ?interactive
    ?obsoletes
    here
    symbol
    t
    (fun args ->
       Async_ecaml.Private.block_on_async ~timeout (fun () -> apply t args);
       Value.nil)
;;
