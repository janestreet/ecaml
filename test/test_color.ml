open! Core_kernel
open! Async_kernel
open! Import
open! Color

let show t =
  print_s
    [%message
      ""
        ~color:(t : t)
        ~is_gray:(is_gray t : bool)
        ~is_defined:(is_defined t : bool)
        ~is_supported:(is_supported t : bool)
        ~rgb:(rgb_exn t : RGB.t)]
;;

let%expect_test "[defined]" =
  print_s [%sexp (defined () : t list)];
  [%expect {| (black blue cyan green magenta red white yellow) |}];
  return ()
;;

let%expect_test "attributes" =
  List.iter ~f:show [ black; blue; cyan; green; magenta; red; white; yellow ];
  [%expect
    {|
    ((color        black)
     (is_gray      true)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 0)
       (g 0)
       (b 0))))
    ((color        blue)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 0)
       (g 0)
       (b 65_535))))
    ((color        cyan)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 0)
       (g 65_535)
       (b 65_535))))
    ((color        green)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 0)
       (g 65_535)
       (b 0))))
    ((color        magenta)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 65_535)
       (g 0)
       (b 65_535))))
    ((color        red)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 65_535)
       (g 0)
       (b 0))))
    ((color        white)
     (is_gray      true)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 65_535)
       (g 65_535)
       (b 65_535))))
    ((color        yellow)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 65_535)
       (g 65_535)
       (b 0)))) |}];
  return ()
;;

let%expect_test "[rgb_exn] raise" =
  require_does_raise [%here] (fun () -> rgb_exn ("zzz" |> of_name));
  [%expect {| ("[Color.rgb_exn] got non-displayable color" (color zzz)) |}];
  return ()
;;

let%expect_test "[of_rgb]" =
  let f = (1 lsl 16) - 1 in
  let ooo = 0, 0, 0
  and foo = f, 0, 0
  and ofo = 0, f, 0
  and oof = 0, 0, f
  and fff = f, f, f in
  List.iter [ ooo; foo; ofo; oof; fff ] ~f:(fun (r, g, b) -> show (of_rgb { r; g; b }));
  [%expect
    {|
    ((color        #000000000000)
     (is_gray      true)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 0)
       (g 0)
       (b 0))))
    ((color        #FFFF00000000)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 65_535)
       (g 0)
       (b 0))))
    ((color        #0000FFFF0000)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 0)
       (g 65_535)
       (b 0))))
    ((color        #00000000FFFF)
     (is_gray      false)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 0)
       (g 0)
       (b 65_535))))
    ((color        #FFFFFFFFFFFF)
     (is_gray      true)
     (is_defined   true)
     (is_supported true)
     (rgb (
       (r 65_535)
       (g 65_535)
       (b 65_535)))) |}];
  return ()
;;
