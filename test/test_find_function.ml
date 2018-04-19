open! Core_kernel
open! Import
open! Find_function

module Q = struct
  include Q
  let find_function                    = "find-function"                    |> Symbol.intern
end

let%expect_test "[find_function]" =
  Echo_area.inhibit_messages (fun () ->
    find_function Q.find_function);
  print_s [%sexp (Current_buffer.get () : Buffer.t)];
  [%expect {|
    "#<buffer find-func.el.gz>" |}];
;;
