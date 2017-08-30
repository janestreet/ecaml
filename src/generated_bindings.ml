external backward_word : int -> bool = "ecaml__backward_word"
let backward_word a0 =
  let r = backward_word a0 in
  Value.Expert.raise_if_emacs_signaled ();
  r
;;

external buffer_live_p : Value.t -> bool = "ecaml__buffer_live_p"
let buffer_live_p a0 =
  let r = buffer_live_p a0 in
  Value.Expert.raise_if_emacs_signaled ();
  r
;;

external elt_returning_int : Value.t -> int -> int = "ecaml__elt_returning_int"
let elt_returning_int a0 a1 =
  let r = elt_returning_int a0 a1 in
  Value.Expert.raise_if_emacs_signaled ();
  r
;;

external forward_word : int -> bool = "ecaml__forward_word"
let forward_word a0 =
  let r = forward_word a0 in
  Value.Expert.raise_if_emacs_signaled ();
  r
;;

external point_max : unit -> int = "ecaml__point_max"
let point_max () =
  let r = point_max () in
  Value.Expert.raise_if_emacs_signaled ();
  r
;;

external point_min : unit -> int = "ecaml__point_min"
let point_min () =
  let r = point_min () in
  Value.Expert.raise_if_emacs_signaled ();
  r
;;

