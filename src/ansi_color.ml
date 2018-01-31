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

let max_supported_code = 109

let customization_group = "ansi-colors" |> Customization.Group.of_string

module Brightness = struct
  type t =
    | Bright
    | Faint
    | Regular
  [@@deriving compare, hash, sexp_of]

  let make_fainter = function
    | Bright  -> Regular
    | Faint   -> Faint
    | Regular -> Faint
  ;;
end

module Bounded_int(Config : sig
    val max_value : int
    val name : string
  end) () : sig
  type t [@@deriving compare, hash, sexp]

  val _min_value : t
  val max_value : t

  val of_int_exn : int -> t
  val to_int : t -> int

end =
struct
  let min_value = 0
  let max_value = Config.max_value

  include Validated.Make_bin_io_compare_hash_sexp (struct
      let here = [%here]

      type t = int [@@deriving sexp, compare, hash, bin_io]

      let validate color_index =
        if min_value <= color_index && color_index <= max_value
        then Validate.pass
        else Validate.fail_s [%message
               (sprintf "%s not between 0 and %d" Config.name max_value)
                 (color_index : int)]
      ;;

      let validate_binio_deserialization = true
    end)

  let to_int = raw

  let of_int_exn = create_exn

  let compare t1 t2 = Int.compare (t1 |> to_int) (t2 |> to_int)

  let _min_value = create_exn min_value
  let max_value = create_exn max_value
end

module Color_index_standard = Bounded_int(struct
    let max_value = 7
    let name = "color index"
  end) ()

(** 8-bit color component value *)
module Color_value_8bit = Bounded_int(struct
    let max_value = 255
    let name = "color value"
  end) ()

module Color_index_256 = Bounded_int(struct
    let max_value = 255
    let name = "color index"
  end) ()

module By_color_index : sig
  type 'a t [@@deriving sexp_of]

  val create_exn : 'a array -> 'a t

  val get : 'a t -> Color_index_standard.t -> 'a

end = struct
  type 'a t = 'a array [@@deriving sexp_of]

  let create_exn array =
    if Array.length array <> Color_index_standard.(max_value |> to_int) + 1
    then raise_s [%message
           "[By_color_index.create_exn] got array of wrong length"
             (array : _ Array.t)];
    array
  ;;

  let get t color_index = t.( color_index |> Color_index_standard.to_int )

end

module Color_spec = struct

  module Standard = struct
    type t =
      { brightness  : Brightness.t
      ; color_index : Color_index_standard.t }
    [@@deriving compare, hash, sexp_of]
  end

  type t =
    | Standard of Standard.t
    | Indexed_256 of Color_index_256.t
    | Rgb of {
        r : Color_value_8bit.t;
        g : Color_value_8bit.t;
        b : Color_value_8bit.t;
      }
  [@@deriving compare, hash, sexp_of]

  let make_fainter t =
    match t with
    | Standard t ->
      Standard { t with brightness = t.brightness |> Brightness.make_fainter }
    | x ->
      x
  ;;
end

module Colors : sig
  type t [@@deriving sexp_of]

  val get : unit -> t
  val color : t -> Color_spec.t -> Color.t
  val faint_default_color : t -> Color.t
end = struct

  let defcustom_color_var here symbol ~standard_value ~docstring =
    let standard_value =
      standard_value
      |> List.map ~f:Value.of_utf8_bytes
      |> Vector.of_list
      |> Vector.to_value
    in
    Customization.defcustom here symbol
      (Vector (List.init 8 ~f:(fun _ -> Customization.Type.Color)))
      ~docstring
      ~group:customization_group
      ~standard_value
  ;;

  let () =
    defcustom_color_var [%here] Q.ansi_color_bright_vector
      ~docstring:"Bright colors used to color escape sequences \
                  90-97 (foreground) and 100-107 (background)"
      ~standard_value:
        [ "grey50"
        ; "red1"
        ; "green1"
        ; "yellow1"
        ; "deep sky blue"
        ; "magenta1"
        ; "cyan1"
        ; "white"]
  ;;

  let () =
    defcustom_color_var [%here] Q.ansi_color_faint_vector
      ~docstring:"Dimmed colors used to color foreground with escape sequence 2"
      ~standard_value:
        [ "black"
        ; "dark red"
        ; "dark green"
        ; "yellow4"
        ; "dark blue"
        ; "magenta4"
        ; "cyan4"
        ; "grey50" ]
  ;;

  type t =
    { regular             : Color.t By_color_index.t
    ; bright              : Color.t By_color_index.t
    ; faint               : Color.t By_color_index.t
    ; faint_default_color : Color.t }
  [@@deriving fields, sexp_of]

  let rgb6 index =
    let b = index mod 6 in
    let index = index / 6 in
    let g = index mod 6 in
    let index = index / 6 in
    let r = index mod 6 in
    let index = index / 6 in
    assert (index = 0);
    let r = r * 255 / 5 in
    let g = g * 255 / 5 in
    let b = b * 255 / 5 in
    Color.of_rgb8 ~r ~g ~b

  let grayscale24 index =
    assert (index < 24);
    assert (index >= 0);
    let value = index * 255 / 23 in
    Color.of_rgb8 ~r:value ~g:value ~b:value

  let color t color_spec =
    let standard_color ~brightness ~color_index =
      let by_color_index =
        match (brightness : Brightness.t) with
        | Bright  -> t.bright
        | Faint   -> t.faint
        | Regular -> t.regular
      in
      By_color_index.get by_color_index color_index
    in
    match (color_spec : Color_spec.t) with
    | Standard { brightness; color_index; } ->
      standard_color ~brightness ~color_index
    | Indexed_256 color_index ->
      let color_index = Color_index_256.to_int color_index in
      (match color_index with
       | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 ->
         standard_color ~brightness:Regular
           ~color_index:(Color_index_standard.of_int_exn color_index)
       | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 ->
         standard_color ~brightness:Bright
           ~color_index:(Color_index_standard.of_int_exn (color_index - 8))
       | color_index ->
         let color_index = color_index - 16 in
         if color_index < 6 * 6 * 6
         then
           rgb6 color_index
         else
           let color_index = color_index - 6 * 6 * 6 in
           grayscale24 color_index)
    | Rgb { r; g; b; } ->
      let r = Color_value_8bit.to_int r in
      let g = Color_value_8bit.to_int g in
      let b = Color_value_8bit.to_int b in
      Color.of_rgb8 ~r ~g ~b
  ;;

  let transform c ~f = Color.rgb_exn c |> Color.RGB.map ~f |> Color.of_rgb

  let make_faint = transform ~f:(fun v -> v / 2)

  let get () =
    Feature.require Q.ansi_color;
    let load_color_vector symbol =
      try
        Current_buffer.value_exn (Var.create symbol (Vector.type_ Color.type_))
        |> By_color_index.create_exn
      with exn ->
        raise_s [%message
          "[Colors.get] unable to load color vector"
            ~symbol:(Symbol.name symbol : string)
            ~_:(exn : exn)]
    in
    let regular = load_color_vector Q.ansi_color_names_vector  in
    let bright  = load_color_vector Q.ansi_color_bright_vector in
    let faint   = load_color_vector Q.ansi_color_faint_vector  in
    let faint_default_color =
      match Face.attribute_value Face.default Foreground with
      | Color c -> make_faint c
      | Unspecified ->
        (* This should only happen in test; just use white as default. *)
        Color.of_name "grey50"
    in
    { regular
    ; bright
    ; faint
    ; faint_default_color }
end

module Code : sig
  type t =
    (* there are some single-code color specs; those currently come with [Single_code]
       constructor and not with [Set_color] *)
    | Single_code of int
    | Set_color of {
        subject : [`background | `foreground];
        color : Color_spec.t;
      }
  [@@deriving compare, hash, sexp_of]

  module Incomplete_param : sig
    type t [@@deriving compare, hash, sexp_of]
    val empty : t
  end

  type parse_result =
    | Done of t
    | Incomplete of Incomplete_param.t
    | Invalid of Error.t

  val feed : Incomplete_param.t -> int -> parse_result
end = struct
  module T = struct
    type t =
      (* there are some single-code color specs; those come with [Single_code]
         constructor *)
      | Single_code of int
      | Set_color of {
          subject : [`background | `foreground];
          color : Color_spec.t;
        }
    [@@deriving compare, hash, sexp_of]
  end
  include T
  include Comparable.Make_plain(T)
  include Hashable.Make_plain(T)

  module Incomplete_param = struct
    type t =
      | Complex_color of [ `background | `foreground ]
      | Rgb_color of {
          subject : [ `background | `foreground ];
          (* incomplete list of rgb components of length up to 2 (3 would be complete) *)
          components : Color_value_8bit.t list;
        }
      | Indexed_256_color of [ `background | `foreground ]
      | None
    [@@deriving compare, sexp_of, hash]

    let empty = None
  end

  type parse_result =
    | Done of t
    | Incomplete of Incomplete_param.t
    | Invalid of Error.t

  let feed incomplete code = match (incomplete : Incomplete_param.t) with
    | None -> (match code with
      | 38 -> Incomplete (Complex_color `foreground)
      | 48 -> Incomplete (Complex_color `background)
      | code -> Done (Single_code code))
    | Complex_color subject ->
      (match code with
       | 5 -> Incomplete (Indexed_256_color subject)
       | 2 -> Incomplete (Incomplete_param.Rgb_color { subject; components = []; })
       | _ -> Invalid (
         Error.create_s [%message
           "invalid 8-bit or 24-bit color escape code: should start with '2' or '5'"]))
    | Rgb_color { subject; components; } ->
      (match Color_value_8bit.of_int_exn code with
       | exception exn ->
         Invalid (Error.of_exn exn)
       | value ->
         let components = components @ [ value ] in
         (match components with
          | [ r; g; b; ] ->
            Done (Set_color { subject; color = Rgb { r; g; b; }; })
          | _ :: _ :: _ :: _ :: _ ->
            failwith "impossible: too many rgb components"
          | [] | [ _ ] | [ _; _; ] ->
            Incomplete (Rgb_color { subject; components; })))
    | Indexed_256_color subject ->
      (match Color_index_256.of_int_exn code with
       | exception exn ->
         Invalid (Error.of_exn exn)
       | color ->
         Done (Set_color { subject; color = Indexed_256 color; }))

end

module Attributes : sig
  type t [@@deriving sexp_of, compare, hash]

  include Equal.S with type t := t

  val empty : t
  val transition : t -> code:Code.t -> t
  val text_properties : t -> Colors.t -> Text.Property.t list
end = struct

  type t =
    { background    : Color_spec.t option
    ; blink_rapid   : bool
    ; blink_slow    : bool
    ; bold          : bool
    ; faint         : bool
    ; foreground    : Color_spec.t option
    ; italic        : bool
    ; reverse_video : bool
    ; underline     : bool
    }
  [@@deriving compare, fields, sexp_of, hash]

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
    ; underline      = false
    }
  ;;

  let to_face_spec t colors : Text.Face_spec.t =
    let attr_val attr v = Face.Attribute_and_value.T (attr, v) in
    let t, init =
      if not t.faint
      then t, []
      else
        match t.foreground with
        | None -> t, [ attr_val Foreground (Color (Colors.faint_default_color colors)) ]
        | Some c ->
          { t with foreground = Some (Color_spec.make_fainter c) }, [] in
    let t =
      if not t.reverse_video
      then t
      else
        { t with
          foreground = t.background
        ; background = t.foreground } in
    let w f acc field =
      match f (Field.get field t) with
      | None -> acc
      | Some (attr, v) ->
        Face.Attribute_and_value.T (attr, v) :: acc
    in
    let add_if_true attribute value =
      w (fun v ->
        if v
        then Some (attribute, value)
        else None)
    in
    let add_color color_attribute f =
      w (Option.map ~f:(fun color_spec ->
        color_attribute, f (Colors.color colors color_spec)))
    in
    let skip acc _ = acc in
    Fields.fold ~init
      ~background:(add_color Background (fun c -> Color c))
      ~blink_rapid:skip
      ~blink_slow:skip
      ~bold:(add_if_true Weight Bold)
      ~faint:skip (* handled above *)
      ~foreground:(add_color Foreground (fun c -> Color c))
      ~italic:(add_if_true Slant Italic)
      ~reverse_video:skip (* handled above *)
      ~underline:(add_if_true Underline Foreground)
    |> function
    | [] -> []
    | attrs -> [ Text.Face_spec.One.Attributes attrs ]
  ;;

  let text_properties t colors : Text.Property.t list =
    let face_spec = to_face_spec t colors |> Text.Face_spec.normalize in
    if List.is_empty face_spec
    then []
    else [ T (Text.Property_name.face           , face_spec)
         ; T (Text.Property_name.font_lock_face , face_spec) ]
  ;;

  let color_spec_standard ~brightness ~code ~offset =
    Color_spec.Standard {
      brightness; color_index = (code - offset) |> Color_index_standard.of_int_exn }
  ;;

  let background_standard t ~brightness ~code ~offset =
    { t with background = Some (color_spec_standard ~brightness ~code ~offset) }
  ;;

  let foreground_standard t ~brightness ~code ~offset =
    { t with foreground = Some (color_spec_standard ~brightness ~code ~offset) }
  ;;

  let set_color ~subject t color = match subject with
    | `foreground ->
      { t with foreground = Some color; }
    | `background ->
      { t with background = Some color; }

  let transition t ~(code : Code.t) =
    (* [(find-function 'ansi-color-apply-sequence)]
       https://en.wikipedia.org/wiki/ANSI_escape_code *)
    match code with
    | Set_color { subject; color; } ->
      set_color ~subject t color
    | Single_code code ->
      match code with
      |  0 -> empty
      |  1 -> { t with bold          = true }
      |  2 -> { t with faint         = true }
      |  3 -> { t with italic        = true }
      |  4 -> { t with underline     = true }
      |  5 -> { t with blink_slow    = true }
      |  6 -> { t with blink_rapid   = true }
      |  7 -> { t with reverse_video = true }
      | 21 -> { t with bold          = false }
      | 22 -> { t with bold          = false }
      | 23 -> { t with italic        = false }
      | 24 -> { t with underline     = false }
      | 25 -> { t with blink_slow    = false; blink_rapid = false }
      | 27 -> { t with reverse_video = false }
      | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 ->
        foreground_standard t ~brightness:Regular ~code ~offset:30
      | 38 -> assert false (* not single-code: handled by [Set_color] *)
      | 39 -> { t with foreground = None }
      | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 ->
        background_standard t ~brightness:Regular ~code ~offset:40
      | 48 -> assert false (* not single-code: handled by [Set_color] *)
      | 49 -> { t with background = None }
      | 90 | 91 | 92 | 93 | 94 | 95 | 96 | 97 ->
        foreground_standard t ~brightness:Bright ~code ~offset:90
      | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 ->
        background_standard t ~brightness:Bright ~code ~offset:100
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

  val transition : t -> raw_code:int -> [ `Ok | `Invalid_escape | `Incomplete_escape ]
  val reset : t -> unit

  val apply_current_state : t -> to_:Region.t -> unit

  val add_text_properties_in_current_buffer : t -> unit
end = struct
  module State = struct

    module Attributes_state = struct
      module T = struct
        type t =
          | Complete of Attributes.t
          | Incomplete_escape of Attributes.t * Code.Incomplete_param.t
          | Invalid_escape
        [@@deriving compare, hash, sexp_of]
      end
      include T
      include Hashable.Make_plain(T)

      let empty = Complete Attributes.empty

      let text_properties t colors = match t with
        | Invalid_escape | Incomplete_escape _ -> []
        | Complete attributes -> Attributes.text_properties attributes colors
    end

    type t =
      { attributes_state : Attributes_state.t
      ; next_state_by_code : t Int.Table.t
      ; mutable regions    : Region.t list
      ; text_properties    : Text.Property.t list }

    let sexp_of_next_state_by_code h =
      List.map (Hashtbl.to_alist h |> List.sort ~cmp:(Comparable.lift Int.compare ~f:fst))
        ~f:(fun (i, next) -> (i, next.attributes_state))
      |> [%sexp_of: (int * Attributes_state.t) list]
    ;;

    let sexp_of_t {
      attributes_state; next_state_by_code;
      regions; text_properties } =
      [%message.omit_nil
        ""
          (attributes_state : Attributes_state.t)
          (text_properties : Text.Property.t list)
          (next_state_by_code : next_state_by_code)
          (regions : Region.t list)]
    ;;

    let create attributes_state text_properties =
      { attributes_state
      ; next_state_by_code = Int.Table.create ~size:4 ()
      ; text_properties
      ; regions            = [] }
    ;;

    let empty () = create Attributes_state.empty []

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
    { mutable all_states    : State.t State.Attributes_state.Table.t
    ; colors                : Colors.t
    ; mutable current_state : State.t
    ; empty_state           : State.t }

  let sexp_of_t { all_states; colors = _; current_state; empty_state } =
    let all_states =
      Hashtbl.data all_states
      |> List.sort ~cmp:(Comparable.lift State.Attributes_state.compare
                           ~f:(fun x -> x.State.attributes_state))
    in
    [%message.omit_nil
      ""
        (current_state : State.t)
        (empty_state : State.t)
        (all_states : State.t list)]
  ;;

  let create () =
    let empty_state = State.empty () in
    { all_states    =
        State.Attributes_state.Table.of_alist_exn ~size:16
          [empty_state.attributes_state, empty_state]
    ; colors        = Colors.get ()
    ; current_state = empty_state
    ; empty_state }
  ;;

  let apply_current_state t ~to_:region = State.add_region t.current_state region

  let add_text_properties_in_current_buffer t =
    Hashtbl.iter t.all_states
      ~f:State.add_text_properties_in_current_buffer
  ;;

  let reset t = t.current_state <- t.empty_state

  let transition t ~raw_code =
    let current_state = t.current_state in
    let next_state =
      (* the horrible [find_exn] is here so we can avoid allocating anything in the common
         case *)
      match Hashtbl.find_exn current_state.next_state_by_code raw_code with
      | state -> state
      | exception Not_found ->
        let incomplete_param, current_attributes =
          match current_state.attributes_state with
          | Complete attributes -> Code.Incomplete_param.empty, attributes
          | Invalid_escape -> Code.Incomplete_param.empty, Attributes.empty
          | Incomplete_escape (attributes, incomplete_param) ->
            incomplete_param, attributes
        in
        let next_attributes_state : State.Attributes_state.t =
          match Code.feed incomplete_param raw_code with
          | Done code ->
            Complete (Attributes.transition current_attributes ~code)
          | Incomplete incomplete_param ->
            Incomplete_escape (current_attributes, incomplete_param)
          | Invalid _ ->
            Invalid_escape
        in
        let next_state =
          Hashtbl.find_or_add t.all_states next_attributes_state
            ~default:(fun () ->
              State.create
                next_attributes_state
                (State.Attributes_state.text_properties next_attributes_state t.colors))
        in
        Hashtbl.set current_state.next_state_by_code ~key:raw_code ~data:next_state;
        next_state
    in
    t.current_state <- next_state;
    match t.current_state.attributes_state with
    | Invalid_escape -> `Invalid_escape
    | Complete _ -> `Ok
    | Incomplete_escape _ -> `Incomplete_escape
  ;;

end

let digit c = Char.to_int c - Char.to_int '0'

type t =
  { mutable append_start : int
  ; mutable escape_start : int
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
  ; escape_start  = 0
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
    Out_channel.output_substring t.out_channel ~buf:t.input ~pos:t.append_start ~len;
    State_machine.apply_current_state t.state_machine ~to_:{ pos = t.output_pos; len };
    t.output_pos <- t.output_pos + len);
;;

let append_string_to_output t string =
  let len = String.length string in
  if len > 0
  then (
    Out_channel.output_string t.out_channel string;
    State_machine.apply_current_state t.state_machine ~to_:{ pos = t.output_pos; len };
    t.output_pos <- t.output_pos + len);
;;

let [@inline always] get_char t =
  let c = t.input.[ t.input_pos ] in
  t.input_pos <- t.input_pos + 1;
  c
;;

let transition t ~raw_code = State_machine.transition t.state_machine ~raw_code

let show_invalid_escapes =
  Var.create ("ansi-color-show-invalid-escapes" |> Symbol.intern) Value.Type.bool
;;

let () =
  Customization.defcustom [%here] show_invalid_escapes.symbol Boolean
    ~docstring:""
    ~group:customization_group
    ~standard_value:Value.t
;;

(* [start_normal], [normal], [escape], and [code], are a custom DFA for processing text
   containing ANSI escape sequences. *)
let rec start_normal t =
  t.append_start <- t.input_pos;
  normal t
and normal t =
  if t.input_pos = t.input_length
  then append_to_output t ~input_offset:0
  else (
    match get_char t with
    | '\027' ->
      append_to_output t ~input_offset:1;
      t.escape_start <- t.input_pos - 1;
      if t.input_pos = t.input_length
      then invalid_escape t
      else (
        match get_char t with
        | '[' ->
          t.saw_escape <- true;
          escape t
        | _ -> invalid_escape t)
    | _ -> normal t)
and escape t = code t 0
and code t ac =
  if t.input_pos = t.input_length
  then invalid_escape t
  else (
    match get_char t with
    | '0' .. '9' as c -> code t (ac * 10 + digit c)
    | ';' ->
      begin match transition t ~raw_code:ac with
      | `Ok | `Incomplete_escape -> escape t
      | `Invalid_escape -> invalid_escape t
      end
    | 'm' ->
      begin match transition t ~raw_code:ac with
      | `Ok -> start_normal t
      | `Invalid_escape | `Incomplete_escape -> invalid_escape t
      end
    | _ -> invalid_escape t)
and invalid_escape t =
  t.input_pos <- t.input_pos - 1;
  State_machine.reset t.state_machine;
  let invalid_escape_sequence =
    String.sub t.input ~pos:t.escape_start ~len:(t.input_pos - t.escape_start) in
  append_string_to_output t
    (if Current_buffer.value_exn show_invalid_escapes
     then (sprintf "<invalid ANSI escape sequence %S>" invalid_escape_sequence)
     else invalid_escape_sequence);
  start_normal t
;;

let print_state_machine = ref false

let color_entire_buffer () =
  let before = Time_ns.now () in
  let buffer_was_modified = Current_buffer.is_modified () in
  let input = Current_buffer.contents () |> Text.to_utf8_bytes in
  let t = create ~input in
  start_normal t;
  if !print_state_machine
  then print_s [%message "" ~state_machine:(t.state_machine : State_machine.t)];
  Out_channel.close t.out_channel;
  if t.saw_escape
  then (
    let show_messages = String.length t.input >= 1_000_000 in
    let message = "Colorizing ..." in
    if show_messages then Echo_area.message message;
    Current_buffer.(set_value read_only) false;
    Current_buffer.set_undo_enabled false;
    Current_buffer.erase ();
    Current_buffer.set_multibyte false;
    Point.insert_file_contents_exn t.output_file;
    Point.goto_min ();
    State_machine.add_text_properties_in_current_buffer t.state_machine;
    Current_buffer.set_multibyte true;
    Current_buffer.(set_value read_only) true;
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
