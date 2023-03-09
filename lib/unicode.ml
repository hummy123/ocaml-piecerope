(* Functions for counting code points and line breaks. *)

(** Given the first byte of a UTF-8 code point, returns the length of that character in UTF-8. *) 
let utf8_length chr =
  match chr with
  | '\x00' .. '\x7f' -> 1 
  | '\xc0' .. '\xdf' -> 2
  | '\xe0' .. '\xef' -> 3
  | '\xf0' .. '\xf7' -> 4
  | _ -> failwith "invalid utf-8 start"

(** Given the first byte of a UTF-8 code point, returns the length of that character in UTF-16. *) 
let utf16_length chr =
  match chr with
  | '\x00' .. '\xef' -> 1
  | '\xf0' .. '\xf7' -> 2
  | _ -> failwith "invalid utf-8 start"

(** Counts the length of the string in UTF-16 and Unicode code points, 
    and builds an array of line breaks in terms of UTF-8 as that is OCaml's native string encoding. *)
let count_string_stats (str: string) (pcStart: int) =
  let rec get utf8Pos utf16Cntr utf32Cntr lineBreaks prevIsCr =
    if utf8Pos >= String.length str then
      utf16Cntr, utf32Cntr, lineBreaks |> List.rev |> Array.of_list
    else
      let chr = String.unsafe_get str utf8Pos in
      let utf8Length = utf8_length chr in
      let utf16Length = utf16_length chr in

      let lineBreaks =
        if chr = '\r' || (chr = '\n' && not prevIsCr) then
          (utf32Cntr + pcStart)::lineBreaks
        else
          lineBreaks
      in
      get (utf8Pos + utf8Length) (utf16Cntr + utf16Length) (utf32Cntr + 1) lineBreaks (chr = '\n')
  in
  get 0 0 0 [] false

(* Gets a substring from a string that has a non-ASCII character inside it.*)
let utf32_sub (str: string) (start: int) (length: int) =
  let finish = start + length in
  let rec sub strPos cdPos strStart strFinish =
    if strPos = String.length str then
      String.sub str strStart (String.length str - strStart)
    else
      let strStart = 
        if cdPos = start then
          strPos
        else
          strStart
      in
      if cdPos = finish then
        String.sub str strStart (strPos - strStart)
      else
        let utf8Length = utf8_length (String.unsafe_get str strPos) in
        sub (strPos + utf8Length) (cdPos + 1) strStart strFinish
  in
  sub 0 0 0 0

type index_offsets = { utf8_pos: int; utf16_pos: int; utf32_pos: int; }

let create_offset utf8_pos utf16_pos utf32_pos = 
  { utf8_pos; utf16_pos; utf32_pos; }

type encoding = Utf8 | Utf16 | Utf32

let count_to (str: string) (countTo: int) (enc: encoding) =
  let rec cnt utf8Pos utf16Pos utf32Pos =
    match enc with
    | Utf8 when utf8Pos = countTo ->
      { utf8_pos = utf8Pos; utf16_pos = utf16Pos; utf32_pos = utf32Pos; }
    | Utf16 when utf16Pos = countTo ->
      { utf8_pos = utf8Pos; utf16_pos = utf16Pos; utf32_pos = utf32Pos; }
    | Utf32 when utf32Pos = countTo ->
      { utf8_pos = utf8Pos; utf16_pos = utf16Pos; utf32_pos = utf32Pos; }
    | _ ->
      let chr = String.unsafe_get str utf8Pos in
      let utf8Length = utf8_length chr in
      let utf16Length = utf16_length chr in

      let nextUtf8 = utf8Pos + utf8Length in
      let nextUtf16 = utf16Pos + utf16Length in
      let nextUtf32 = utf32Pos + 1 in
      (* Match to see if next pos is over and clip to current if so. Else, recurse. *)
      (match enc with
      | Utf8 when nextUtf8 > countTo ->
        { utf8_pos = utf8Pos; utf16_pos = utf16Pos; utf32_pos = utf32Pos; }
      | Utf16 when nextUtf16 > countTo ->
        { utf8_pos = utf8Pos; utf16_pos = utf16Pos; utf32_pos = utf32Pos; }
      | _ ->
        cnt nextUtf8 nextUtf16 nextUtf32)
  in
  cnt 0 0 0

