let char_bytes chr =
  match chr with
  | '\x00' .. '\x7f' -> 1
  | '\xc0' .. '\xdf' -> 2
  | '\xe0' .. '\xef' -> 3
  | '\xf0' .. '\xf7' -> 4
  | _ -> failwith "invalid utf-8 start"

let get_char_int str pos length =
  match length with
  | 1 ->
      String.unsafe_get str pos |> int_of_char
  | 2 ->
      (String.unsafe_get str pos |> int_of_char) +
      (String.unsafe_get str (pos + 1) |> int_of_char)
  | 3 ->
      (String.unsafe_get str pos |> int_of_char) +
      (String.unsafe_get str (pos + 1) |> int_of_char) +
      (String.unsafe_get str (pos + 2) |> int_of_char)
  | 4 ->
      (String.unsafe_get str pos |> int_of_char) +
      (String.unsafe_get str (pos + 1) |> int_of_char) +
      (String.unsafe_get str (pos + 2) |> int_of_char) +
      (String.unsafe_get str (pos + 3) |> int_of_char)
  | _ -> failwith "String_processor.get_char_int error"

let get_line_breaks_and_lookup_table (str: string) =
  let rec get pos riCounter acc =
    let leftLegnth = char_bytes (String.unsafe_get str pos) in
    let leftChar = get_char_int str pos leftLegnth in
    let leftCategory = Codepoint_converter.intToCategory leftChar in
  
    let rightStart = pos + leftLegnth in
    if rightStart < String.length str then
      let rightLength = char_bytes (String.unsafe_get str rightStart) in
      let rightChar = get_char_int str rightStart rightLength in
      let rightCategory = Codepoint_converter.intToCategory rightChar in

      let open Codepoint_converter in
      match leftCategory, rightCategory with
      (* Do not break between CR LF *)
      | CR, LF -> 
          get rightStart 0 acc
      (* Otherwise, break between controls. *)
      | (Control | CR | LF), _ 
      | _, (Control | CR | LF) -> 
          get rightStart 0 (rightStart::acc)
      (* Do not break between Hangul syllable sequences. *)
      | L, (L | V | LV | LVT)
      | (LV | V), (V | T)
      | (LVT | T), T->
          get rightStart 0 acc
      (* Do not break between extending characters or ZWJ. *)
      | _, (Extend | ZWJ) ->
          get rightStart 0 acc
      (* Do not break before SpacingMarks. *)
      | _, SpacingMark ->
          get rightStart 0 acc
      (* Do not break after Prepend characters. *)
      | Prepend, _ ->
          get rightStart 0 acc
      (* Do not break within emoji modifier sequences or emoji zwj sequences. *)
      | (Extend | ZWJ | Extended_Pictographic), Extended_Pictographic ->
          get rightStart 0 acc
      (* Do not break emoji flag sequences if there is an odd number of regional indicators before.. *)
      | Regional_Indicator, Regional_Indicator when riCounter mod 2 = 1 ->
          get rightStart (riCounter + 1) acc
      | _, _ ->
          get rightStart 0 (rightStart::acc)
    else
      acc
  in
  get 0 0 [0]
