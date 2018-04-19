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

module Q = struct
  include Q
  let ansi_color                       = "ansi-color"                       |> Symbol.intern
  let ansi_color_bright_vector         = "ansi-color-bright-names-vector"   |> Symbol.intern
  let ansi_color_faint_vector          = "ansi-color-faint-names-vector"    |> Symbol.intern
  let ansi_color_names_vector          = "ansi-color-names-vector"          |> Symbol.intern
end

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

module Add_text_properties : sig
  type t
  val empty : t
  val create : Text.Property.t list -> t
  val apply : t -> (start : int -> end_ : int -> unit)
  val is_empty : t -> bool
end = struct
  type t =
    | Empty
    | Non_empty of (start : int -> end_ : int -> unit)

  let empty = Empty

  let create = function
    | [] -> Empty
    | (_ :: _) as t ->
      Non_empty (Staged.unstage (Current_buffer.add_text_properties_staged t))

  let apply t ~start ~end_ =
    match t with
    | Empty -> ()
    | Non_empty f -> f ~start ~end_

  let is_empty =
    function
    | Empty -> true
    | Non_empty _ -> false
end

module State_machine : sig
  type t [@@deriving sexp_of]

  module State : sig
    type t [@@deriving sexp_of]
  end
  val create : unit -> t

  val transition : t -> raw_code:int -> [ `Ok | `Invalid_escape | `Incomplete_escape ]
  val reset : t -> unit

  val current_text_properties : t -> Add_text_properties.t

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
      ; text_properties : Text.Property.t list
      ; add_text_properties : Add_text_properties.t
      }

    let sexp_of_next_state_by_code h =
      List.map (Hashtbl.to_alist h |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst))
        ~f:(fun (i, next) -> (i, next.attributes_state))
      |> [%sexp_of: (int * Attributes_state.t) list]
    ;;

    let sexp_of_t {
      attributes_state; next_state_by_code;
      text_properties; add_text_properties = _ } =
      [%message.omit_nil
        ""
          (attributes_state : Attributes_state.t)
          (text_properties : Text.Property.t list)
          (next_state_by_code : next_state_by_code)
      ]
    ;;

    let create attributes_state text_properties =
      let add_text_properties = Add_text_properties.create text_properties in
      { attributes_state
      ; next_state_by_code = Int.Table.create ~size:4 ()
      ; text_properties
      ; add_text_properties
      }
    ;;

    let empty () = create Attributes_state.empty []
  end

  type t =
    { mutable all_states    : State.t State.Attributes_state.Table.t
    ; colors                : Colors.t
    ; mutable current_state : State.t
    ; empty_state           : State.t }

  let sexp_of_t { all_states; colors = _; current_state; empty_state } =
    let all_states =
      Hashtbl.data all_states
      |> List.sort ~compare:(Comparable.lift State.Attributes_state.compare
                               ~f:(fun x -> x.State.attributes_state))
    in
    [%message.omit_nil
      ""
        (current_state : State.t)
        (empty_state : State.t)
        (all_states : State.t list)]
  ;;

  let current_state t = t.current_state

  let current_text_properties t = (current_state t).add_text_properties

  let create () =
    let empty_state = State.empty () in
    { all_states    =
        State.Attributes_state.Table.of_alist_exn ~size:16
          [empty_state.attributes_state, empty_state]
    ; colors        = Colors.get ()
    ; current_state = empty_state
    ; empty_state }
  ;;

  let reset t = t.current_state <- t.empty_state

  let transition t ~raw_code =
    let current_state = t.current_state in
    let next_state =
      (* the horrible [find_exn] is here so we can avoid allocating anything in the common
         case *)
      match Hashtbl.find_exn current_state.next_state_by_code raw_code with
      | state -> state
      | exception (Not_found_s _ | Caml.Not_found) ->
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

module Temp_file_state = struct
  type t =
    { out_channel : Out_channel.t
    ; temp_file : string
    ; mutable regions : (Region.t * Add_text_properties.t) list
    }

  let create () =
    let temp_file = Caml.Filename.temp_file "ecaml-ansi-color" "" in
    { out_channel = Out_channel.create temp_file
    ; temp_file
    ; regions = []
    }
end

exception End_of_input

(** Represents the colorization I/O backend.
    It has an input stream, a buffer and a colorized output stream. *)
module Colorization_backend : sig
  type t

  (** Reads the next character from the input stream into the buffer and
      returns it. Raises [End_of_input] if end of input was reached. *)
  val get_char_exn : t -> char

  (** Writes the buffer verbatim to the output (and applies the properties associated
      with the given [State.t]).
      The last [except_for_last] bytes are kept in the buffer. *)
  val keep_verbatim : t -> except_for_last:int -> Add_text_properties.t -> unit

  (** Drops the buffer without writing it to the output. *)
  val drop : t -> unit

  (** Prints an "invalid escape" error message and clears the buffer.
      Before doing so it rewinds the input by 1 byte if possible, with the assumption that
      the last character of the escape sequence might not be intended to be a part of it.
  *)
  val print_invalid_escape : t -> verbose:bool -> unit

  val create :
    mode:[ `Use_temp_file of Temp_file_state.t | `In_place_colorization ]
    -> input:string -> output_pos:int -> t

  val finished : t ->
    [ `Use_temp_file of Temp_file_state.t
    | `In_place_colorization
    ]
end = struct

  type t =
    { mutable output_pos   : int
    ; input                : string
    ; input_length         : int
    ; mutable region_start : int
    ; mutable input_pos    : int
    ; mode :
        [ `Use_temp_file of Temp_file_state.t
        | `In_place_colorization
        ]
    }

  let create ~mode ~input ~output_pos =
    { input
    ; input_length = String.length input
    ; region_start = 0
    ; input_pos = 0
    ; output_pos
    ; mode
    }

  let finished t =
    assert (Int.(=) t.input_length t.input_pos);
    assert (Int.(=) t.input_length t.region_start);
    t.mode
  ;;

  let [@inline always] get_char_exn t =
    if t.input_length = t.input_pos
    then
      raise End_of_input
    else (
      let c = t.input.[ t.input_pos ] in
      t.input_pos <- t.input_pos + 1;
      c
    )
  ;;

  let drop t =
    let len = t.input_pos - t.region_start in
    if len > 0
    then begin
      t.region_start <- t.input_pos;
      match t.mode with
      | `Use_temp_file _ -> ()
      | `In_place_colorization ->
        let start = Position.of_int_exn t.output_pos in
        let end_ = Position.of_int_exn (t.output_pos + len) in
        Current_buffer.delete_region ~start ~end_;
    end
  ;;

  let keep_verbatim t ~except_for_last add_text_properties =
    let len = (t.input_pos - except_for_last) - t.region_start in
    assert (len >= 0);
    if len > 0
    then begin
      let start = t.output_pos in
      let end_  = t.output_pos + len in
      begin match t.mode with
      | `In_place_colorization ->
        Add_text_properties.apply add_text_properties ~start ~end_
      | `Use_temp_file temp_file_state ->
        Out_channel.output_substring temp_file_state.out_channel
          ~buf:t.input ~pos:t.region_start ~len;
        if not (Add_text_properties.is_empty add_text_properties) then
          (let region = { Region.pos = start; len } in
           temp_file_state.regions <-
             (region, add_text_properties) ::
             temp_file_state.regions);
      end;
      t.region_start <- t.region_start + len;
      t.output_pos <- end_;
    end;
  ;;

  let attempt_rewind t =
    if t.input_pos > t.region_start + 1
    then
      t.input_pos <- t.input_pos - 1
  ;;

  let print_invalid_escape t ~verbose =
    attempt_rewind t;
    match verbose with
    | false ->
      keep_verbatim t ~except_for_last:0 Add_text_properties.empty
    | true ->
      let len = t.input_pos - t.region_start in
      let invalid_escape_string = String.sub t.input ~pos:t.region_start ~len in
      let message = sprintf "<invalid ANSI escape sequence %S>" invalid_escape_string in
      drop t;
      begin match t.mode with
      | `Use_temp_file temp_file_state ->
        Out_channel.output_string temp_file_state.out_channel message;
      | `In_place_colorization ->
        Point.goto_char (Position.of_int_exn t.output_pos);
        Point.insert message;
      end;
      t.output_pos <- t.output_pos + String.length message;
  ;;

end

type t =
  { backend : Colorization_backend.t
  ; mutable saw_escape   : bool
  ; state_machine        : State_machine.t
  }

let create ~input ~output_pos ~mode =
  { backend = Colorization_backend.create ~input ~output_pos ~mode
  ; saw_escape    = false
  ; state_machine = State_machine.create ()
  }
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

let keep_verbatim t =
  Colorization_backend.keep_verbatim t.backend
    (State_machine.current_text_properties t.state_machine)
;;

let get_char_exn t = Colorization_backend.get_char_exn t.backend

(* [normal], [escape], and [code], are a custom DFA for processing text
   containing ANSI escape sequences. *)
let rec normal t =
  match get_char_exn t with
  | exception End_of_input ->
    keep_verbatim t ~except_for_last:0
  | '\027' ->
    t.saw_escape <- true;
    keep_verbatim t ~except_for_last:1;
    (match get_char_exn t with
     | exception End_of_input ->
       invalid_escape t
     | '[' ->
       escape t
     | _ ->
       invalid_escape t)
  | _ -> normal t
and escape t = code t 0
and code t ac =
  match get_char_exn t with
  | exception End_of_input ->
    invalid_escape t
  | '0' .. '9' as c -> code t (ac * 10 + digit c)
  | ';' ->
    begin match transition t ~raw_code:ac with
    | `Ok | `Incomplete_escape -> escape t
    | `Invalid_escape -> invalid_escape t
    end
  | 'm' ->
    begin match transition t ~raw_code:ac with
    | `Ok -> Colorization_backend.drop t.backend; normal t
    | `Invalid_escape | `Incomplete_escape -> invalid_escape t
    end
  | _ -> invalid_escape t
and invalid_escape t =
  State_machine.reset t.state_machine;
  Colorization_backend.print_invalid_escape t.backend
    ~verbose:(Current_buffer.value_exn show_invalid_escapes);
  normal t
;;

let print_state_machine = ref false

(* [set_multibyte] on large buffers is very slow, so we do it only when necessary. In the
   case of [use_temp_file = true] we don't need to change multibyte until right before we
   apply the text properties and then only after the region was deleted and buffer is much
   smaller (actually empty in the usual case of coloring the whole buffer) *)
let color_region ~start ~end_ ~use_temp_file =
  let before = Time_ns.now () in
  let is_multibyte = Current_buffer.is_multibyte () in
  let input = Current_buffer.contents () ~start ~end_ |> Text.to_utf8_bytes in
  let show_messages = String.length input >= 1_000_000 in
  let mode, start, end_ =
    if use_temp_file then
      `Use_temp_file (Temp_file_state.create ()), start, end_
    else begin
      let start, end_ =
        (* Positions are not preserved when changing multibyte since they are just ints,
           but markers' positions are updated. So we do this round-about way of getting
           new positions of [start] and [end_] after changing multibyte. *)
        if not is_multibyte then start, end_
        else begin
          let marker_of_position p =
            let m = Marker.create () in
            Current_buffer.set_marker_position m p;
            m
          in
          let position_of_marker m = Marker.position m |> Option.value_exn in
          let start = marker_of_position start in
          let end_  = marker_of_position end_  in
          Current_buffer.set_multibyte false;
          position_of_marker start, position_of_marker end_
        end
      in
      `In_place_colorization, start, end_
    end
  in
  let message = "Colorizing ..." in
  let output_pos = if use_temp_file then 0 else Position.to_int start in
  let t = create ~input ~output_pos ~mode in
  normal t;
  if !print_state_machine
  then print_s [%message "" ~state_machine:(t.state_machine : State_machine.t)];
  if t.saw_escape && show_messages then (Echo_area.message message);
  begin match Colorization_backend.finished t.backend with
  | `In_place_colorization -> ()
  | `Use_temp_file temp_file_state ->
    Out_channel.close temp_file_state.out_channel;
    if t.saw_escape then begin
      Current_buffer.delete_region ~start ~end_;
      Point.goto_char start;
      Current_buffer.set_multibyte false;
      let start = Point.get () |> Position.to_int in
      Point.insert_file_contents_exn temp_file_state.temp_file;
      List.iter temp_file_state.regions ~f:(fun (region, add_text_properties) ->
        Add_text_properties.apply add_text_properties
          ~start:(start + region.pos)
          ~end_:(start + region.pos + region.len))
    end;
    Sys.remove temp_file_state.temp_file;
  end;
  Current_buffer.set_multibyte is_multibyte;
  if show_messages && t.saw_escape && not am_running_inline_test
  then (
    let took = Time_ns.diff (Time_ns.now ()) before in
    Echo_area.message (
      concat [
        message;" done (took "; took |> Time_ns.Span.to_sec |> sprintf "%.3f"; "s)" ]));
  t.saw_escape
;;

let color_region_in_current_buffer ~start ~end_ ?(use_temp_file = false) () =
  Current_buffer.save_excursion (fun () ->
    ignore (color_region ~start ~end_ ~use_temp_file : bool))

let color_current_buffer () =
  let buffer_was_modified = Current_buffer.is_modified () in
  Current_buffer.(set_value read_only) false;
  Current_buffer.set_undo_enabled false;
  ignore (color_region ~start:(Point.min ()) ~end_:(Point.max ()) ~use_temp_file:true : bool);
  if not buffer_was_modified then Current_buffer.set_modified false;
  Current_buffer.(set_value read_only) true;
  Point.goto_char (Point.min ());
;;
