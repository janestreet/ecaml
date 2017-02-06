open Core_kernel
open Async
open Ecaml.Std.Emacs

let cons a b = Symbol.funcall Symbol.cons [ a; b ]

let of_list l ~to_emacs =
  List.fold_right l ~init:Value.nil ~f:(fun elt acc ->
    cons (to_emacs elt) acc)
;;

let () =
  Reader.with_file "example_plugin.ml" ~f:(fun r ->
    Reader.lines r
    |> Pipe.fold_without_pushback ~init:0 ~f:(fun acc _ -> acc + 1)
    >>| printf "%d lines\n%!"
  ) >>> ignore;
  don't_wait_for (
    let%bind contents = Reader.file_contents "jbuild" in
    String.length contents
    |> Core.printf "length is %d\n%!";
    Deferred.unit
  );
  defun ("watch-cpu" |> Symbol.intern) ~args:[] (fun _ ->
    Cpu_usage.samples ()
    |> Pipe.iter_without_pushback ~f:(fun pc ->
      let pc = pc |> Percent.to_string |> Value.of_string in
      funcall_i Symbol.("insert" |> intern |> to_value) [ pc; Value.of_string "\n" ])
    |> don't_wait_for;
    Value.nil);
  defun ("raise-something" |> Symbol.intern) ~args:[] (fun _ ->
    failwith "here you go");
  defun ~args:[] ~rest_arg:(Symbol.intern "a") ("my-list" |> Symbol.intern) (fun a ->
    Array.to_list a
    |> of_list ~to_emacs:Fn.id
  );
  defun ~args:[] ~rest_arg:(Symbol.intern "_") ("should-throw" |> Symbol.intern) (fun _ ->
    try
      Symbol.funcall ("+" |> Symbol.intern) [ Value.of_string "hi" ]
    with
    | exn ->
      Debug.eprint_s [%message "it threw!" (exn : Exn.t)];
      Exn.reraise exn "raising to Emacs");
  message "hello there";
  messagef !"my args are %{sexp: string array}" Sys.argv;
  provide ("example_plugin" |> Symbol.intern);
;;
