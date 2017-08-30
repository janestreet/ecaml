(** An ANSI escape sequence is a sequence of characters in a text file that describes text
    attributes: foreground color, background color, bold, italic, etc.  An ANSI escape
    sequence consists of:

    1. "\027["
    2. a sequence of decimal integer codes separated by ";"
    3. "m"

    For a description of the meaning of the integer codes, see:
    https://en.wikipedia.org/wiki/ANSI_escape_code

    This implementation works by treating a collection of attribute settings as a state in
    a state machine, and an integer code as causing a transition from one state to the
    next.  It visits each character in the input, maintaining the current state as it
    encounters escape sequences.  It records the current state in effect for each ordinary
    input character, and sets a text property for the face corresponding to that state.
    The state machine is constructed from scratch dynamically on demand as states and
    transitions are encountered. *)

open! Core_kernel
open! Import

let max_supported_code = 49

module Ansi_color_map : sig
  type t [@@deriving sexp_of]

  val get : unit -> t

  val get_face_spec : t -> code:int -> Text.Face_spec.One.t
end = struct
  type t = Text.Face_spec.One.t array [@@deriving sexp_of]

  let get_face_spec t ~code = t.( code )

  let get () =
    Feature.require Q.ansi_color;
    let t =
      try
        Symbol.value_exn Q.ansi_color_map
        |> Vector.of_value_exn
        |> Vector.to_array ~f:Text.Face_spec.One.of_value_exn
      with exn ->
        raise_s [%message
          "[Ansi_color_map.get] unable to interpret [ansi-color-map]" ~_:(exn : exn)] in
    let length = Array.length t in
    let min_length = max_supported_code + 1 in
    if length < min_length
    then raise_s [%message
           "[Ansi_color_map.get] got too short [ansi-color-map]"
             (length : int)
             (min_length : int)
             ~ansi_color_map:(t : t)];
    t
  ;;
end

module Attributes : sig
  type t [@@deriving sexp_of]

  include Equal.S with type t := t

  val empty : t
  val transition : t -> code:int -> t
  val text_properties : t -> Ansi_color_map.t -> Text.Property.t list
end = struct
  type t =
    { background    : int option
    ; blink_rapid   : bool
    ; blink_slow    : bool
    ; bold          : bool
    ; faint         : bool
    ; foreground    : int option
    ; italic        : bool
    ; reverse_video : bool
    ; underline     : bool }
  [@@deriving compare, fields, sexp_of]

  let equal = [%compare.equal: t]

  let empty =
    { background     = None
    ; blink_rapid    = false
    ; blink_slow     = false
    ; bold           = false
    ; faint          = false
    ; foreground     = None
    ; italic         = false
    ; reverse_video  = false
    ; underline      = false }
  ;;

  let to_face_spec t ansi_color_map : Text.Face_spec.t =
    let add code ac =
      let face_spec = Ansi_color_map.get_face_spec ansi_color_map ~code in
      let should_keep =
        match face_spec with
        | Face face -> not (Face.equal face Face.default)
        | _ -> true in
      if should_keep then face_spec :: ac else ac in
    let add_if_some ac field =
      match Field.get field t with
      | None -> ac
      | Some code -> add code ac in
    let add_if_true code ac field =
      if Field.get field t
      then add code ac
      else ac in
    Fields.fold ~init:[]
      ~background:     add_if_some
      ~blink_rapid:    (add_if_true 6)
      ~blink_slow:     (add_if_true 5)
      ~bold:           (add_if_true 1)
      ~faint:          (add_if_true 2)
      ~foreground:     add_if_some
      ~italic:         (add_if_true 3)
      ~reverse_video:  (add_if_true 7)
      ~underline:      (add_if_true 4)
  ;;

  let text_properties t ansi_color_map : Text.Property.t list =
    let face_spec = to_face_spec t ansi_color_map |> Text.Face_spec.normalize in
    if List.is_empty face_spec
    then []
    else [ T (Text.Property_name.face           , face_spec)
         ; T (Text.Property_name.font_lock_face , face_spec) ]
  ;;

  let transition t ~code =
    (* [(find-function 'ansi-color-apply-sequence)]
       https://en.wikipedia.org/wiki/ANSI_escape_code *)
    match code with
    |   0 -> empty
    |   1 -> { t with bold          = true }
    |   2 -> { t with faint         = true }
    |   3 -> { t with italic        = true }
    |   4 -> { t with underline     = true }
    |   5 -> { t with blink_slow    = true }
    |   6 -> { t with blink_rapid   = true }
    |   7 -> { t with reverse_video = true }
    |  21 -> { t with bold          = false }
    |  22 -> { t with bold          = false }
    |  23 -> { t with italic        = false }
    |  24 -> { t with underline     = false }
    |  25 -> { t with blink_slow    = false; blink_rapid = false }
    |  27 -> { t with reverse_video = false }
    |  30
    |  31
    |  32
    |  33
    |  34
    |  35
    |  36
    |  37 -> { t with foreground = Some code }
    |  39 -> { t with foreground = None }
    |  40
    |  41
    |  42
    |  43
    |  44
    |  45
    |  46
    |  47 -> { t with background = Some code }
    |  49 -> { t with background = None }
    | _ -> empty
  ;;
end

module Region = struct
  type t =
    { pos : int
    ; len : int }
  [@@deriving sexp_of]
end

module State_machine : sig
  type t [@@deriving sexp_of]

  val create : unit -> t

  val transition : t -> code:int -> unit

  val apply_current_state : t -> to_:Region.t -> unit

  val add_text_properties_in_current_buffer : t -> unit
end = struct
  module State = struct
    type t =
      { attributes         : Attributes.t
      ; next_state_by_code : t option array
      ; mutable regions    : Region.t list
      ; text_properties    : Text.Property.t list }

    let sexp_of_next_state_by_code array =
      Array.foldi array ~init:[] ~f:(fun i ac next ->
        match next with
        | None -> ac
        | Some t -> (i, t.attributes) :: ac)
      |> List.rev
      |> [%sexp_of: (int * Attributes.t) list]
    ;;

    let sexp_of_t { attributes; next_state_by_code; regions; text_properties } =
      [%message.omit_nil
        ""
          (attributes : Attributes.t)
          (text_properties : Text.Property.t list)
          (next_state_by_code : next_state_by_code)
          (regions : Region.t list)]
    ;;

    let create attributes text_properties =
      { attributes
      ; next_state_by_code = Array.create ~len:(max_supported_code + 1) None
      ; text_properties
      ; regions            = [] }
    ;;

    let empty () = create Attributes.empty []

    let add_region t region =
      if not (List.is_empty t.text_properties) then t.regions <- region :: t.regions;
    ;;

    let add_text_properties_in_current_buffer t =
      let set_text_properties =
        unstage (Current_buffer.add_text_properties_staged t.text_properties) in
      List.iter t.regions ~f:(fun { Region. pos; len } ->
        set_text_properties
          ~start:pos
          ~end_: (pos + len));
    ;;
  end

  type t =
    { mutable all_states    : State.t list
    ; ansi_color_map        : Ansi_color_map.t
    ; mutable current_state : State.t
    ; empty_state           : State.t }

  let sexp_of_t { all_states; ansi_color_map = _; current_state; empty_state } =
    [%message.omit_nil
      ""
        (current_state : State.t)
        (empty_state : State.t)
        (all_states : State.t list)]
  ;;

  let create () =
    let empty_state = State.empty () in
    { all_states     = []
    ; ansi_color_map = Ansi_color_map.get ()
    ; current_state  = empty_state
    ; empty_state }
  ;;

  let apply_current_state t ~to_:region = State.add_region t.current_state region

  let add_text_properties_in_current_buffer t =
    List.iter t.all_states ~f:State.add_text_properties_in_current_buffer
  ;;

  let transition t ~code : unit =
    let current_state = t.current_state in
    let next_state =
      match current_state.next_state_by_code.( code ) with
      | exception _ -> t.empty_state
      | Some state -> state
      | None ->
        let current_attributes = current_state.attributes in
        let next_attributes = Attributes.transition current_attributes ~code in
        let next_state =
          if phys_equal next_attributes Attributes.empty
          then t.empty_state
          else if phys_equal next_attributes current_attributes
          then current_state
          else (
            match
              List.find t.all_states ~f:(fun (state : State.t) ->
                Attributes.equal state.attributes next_attributes)
            with
            | Some state -> state
            | None ->
              let state =
                State.create next_attributes
                  (Attributes.text_properties next_attributes t.ansi_color_map) in
              t.all_states <- state :: t.all_states;
              state) in
        current_state.next_state_by_code.( code ) <- Some next_state;
        next_state in
    t.current_state <- next_state;
  ;;
end

let digit c = Char.to_int c - Char.to_int '0'

type t =
  { mutable append_start : int
  ; input                : string
  ; input_length         : int
  ; mutable input_pos    : int
  ; out_channel          : Out_channel.t
  ; output_file          : string
  ; mutable output_pos   : int
  ; mutable saw_escape   : bool
  ; state_machine        : State_machine.t }
[@@deriving sexp_of]

let create ~input =
  let output_file = Caml.Filename.temp_file "ecaml-ansi-color" "" in
  { append_start  = 0
  ; input
  ; input_length  = String.length input
  ; input_pos     = 0
  ; out_channel   = Out_channel.create output_file
  ; output_file
  ; output_pos    = 1
  ; saw_escape    = false
  ; state_machine = State_machine.create () }
;;

let append_to_output t ~input_offset =
  let len = (t.input_pos - input_offset) - t.append_start in
  if len > 0
  then (
    Out_channel.output t.out_channel ~buf:t.input ~pos:t.append_start ~len;
    State_machine.apply_current_state t.state_machine ~to_:{ pos = t.output_pos; len };
    t.output_pos <- t.output_pos + len);
;;

let [@inline always] get_char t =
  let c = t.input.[ t.input_pos ] in
  t.input_pos <- t.input_pos + 1;
  c
;;

let transition t ~code = State_machine.transition t.state_machine ~code

let rec is_escape_sequence t pos =
  pos < t.input_length
  && (
    match t.input.[ pos ] with
    | 'm' -> true
    | '0' .. '9' | ';' -> is_escape_sequence t (pos + 1)
    | _ -> false)
;;

(* [start_normal], [normal], [escape], and [code], are a custom DFA for processing text
   containing ANSI escape sequences. *)
let rec start_normal t =
  t.append_start <- t.input_pos;
  normal t
and normal t =
  if t.input_pos < t.input_length
  then
    match get_char t with
    | '\027' ->
      if t.input_pos < t.input_length
      then (
        match get_char t with
        | '[' ->
          if not (is_escape_sequence t t.input_pos)
          then normal t
          else (
            t.saw_escape <- true;
            append_to_output t ~input_offset:2;
            escape t)
        | _ -> normal t)
    | _ -> normal t
and escape t =
  if t.input_pos < t.input_length
  then (
    match get_char t with
    | '0' .. '9' as c -> code t (digit c)
    | ';' -> escape t
    | 'm' -> start_normal t
    | char -> raise_s [%message "bug" ~pos:(t.input_pos - 1 : int) (char : char)])
and code t ac =
  if t.input_pos < t.input_length
  then (
    match get_char t with
    | '0' .. '9' as c -> code t (ac * 10 + digit c)
    | ';' -> transition t ~code:ac; escape t
    | 'm' -> transition t ~code:ac; start_normal t
    | char -> raise_s [%message "bug" ~pos:(t.input_pos - 1 : int) (char : char)])
;;

let print_state_machine = ref false

let color_entire_buffer () =
  let before = Time_ns.now () in
  let buffer_was_modified = Current_buffer.is_modified () in
  let input = Current_buffer.contents () |> Text.to_utf8_bytes in
  let t = create ~input in
  start_normal t;
  append_to_output t ~input_offset:0;
  if !print_state_machine
  then print_s [%message "" ~state_machine:(t.state_machine : State_machine.t)];
  Out_channel.close t.out_channel;
  if t.saw_escape
  then (
    let show_messages = String.length t.input >= 1_000_000 in
    let message = "Colorizing ..." in
    if show_messages then Echo_area.message message;
    Current_buffer.set_read_only false;
    Current_buffer.set_undo_enabled false;
    Current_buffer.erase ();
    Current_buffer.set_multibyte false;
    Point.insert_file_contents_exn t.output_file;
    Point.(goto_char (min ()));
    State_machine.add_text_properties_in_current_buffer t.state_machine;
    Current_buffer.set_multibyte true;
    Current_buffer.set_read_only true;
    if not buffer_was_modified then Current_buffer.set_modified false;
    if show_messages && not am_running_inline_test
    then (
      let took = Time_ns.diff (Time_ns.now ()) before in
      Echo_area.message (
        concat [
          message;" done (took "; took |> Time_ns.Span.to_sec |> sprintf "%.3f"; "s)" ])));
  Sys.remove t.output_file;
  t.saw_escape
;;

let maybe_color_text text =
  Current_buffer.set_temporarily_to_temp_buffer (fun () ->
    Point.insert_text text;
    let colored = color_entire_buffer () in
    if colored
    then Some (Current_buffer.contents ~text_properties:true ())
    else None)
;;

let color_text text =
  match maybe_color_text text with
  | None -> text
  | Some x -> x
;;

let color_region ~start ~end_ =
  let input = Current_buffer.contents () ~start ~end_ in
  match maybe_color_text input with
  | None -> ()
  | Some colored ->
    Current_buffer.kill_region ~start ~end_;
    Point.goto_char start;
    Point.insert_text colored;
    Point.goto_char start;
;;

let color_current_buffer ?start ?end_ () =
  match start, end_ with
  | None, None -> ignore (color_entire_buffer () : bool);
  | _, _ ->
    color_region
      ~start:(match start with Some x -> x | None -> Point.min ())
      ~end_:( match end_  with Some x -> x | None -> Point.max ())
;;
