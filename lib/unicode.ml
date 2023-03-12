open Piece_types

let create_offsets utf8_pos utf16_pos utf32_pos =
  { utf8_pos; utf16_pos; utf32_pos }

(** Given the first byte of a UTF-8 code point, returns the length of that character in UTF-8. *)
let utf8_length chr =
  match chr with
  | '\x00' .. '\x7f' -> 1
  | '\xc0' .. '\xdf' -> 2
  | '\xe0' .. '\xef' -> 3
  | '\xf0' .. '\xf7' -> 4
  | _ -> failwith "invalid utf-8 start"

(** 
    Given the first byte of a UTF-8 code point, returns the length of that character in UTF-16. 
    This is almost identical to the utf8_length function above and that function can support a tuple,
    but the additional allocation a tuple has bothered me.
 *)
let utf16_length chr =
  match chr with
  | '\x00' .. '\xef' -> 1
  | '\xf0' .. '\xf7' -> 2
  | _ -> failwith "invalid utf-8 start"

(** Counts the length of the string in UTF-16 and Unicode code points, 
    and builds an array of line breaks in terms of UTF-8 as that is OCaml's native string encoding. *)
let count_string_stats (str : string) (buffer_length : int) =
  let rec get utf8_pos utf16_cntr utf32_cntr line_breaks prev_is_cr =
    if utf8_pos >= String.length str then
      (utf16_cntr, utf32_cntr, line_breaks |> List.rev |> Array.of_list)
    else
      let chr = String.unsafe_get str utf8_pos in
      let utf8_length = utf8_length chr in
      let utf16_length = utf16_length chr in

      let line_breaks =
        if chr = '\r' || (chr = '\n' && not prev_is_cr) then
          (utf32_cntr + buffer_length) :: line_breaks
        else line_breaks
      in
      get (utf8_pos + utf8_length)
        (utf16_cntr + utf16_length)
        (utf32_cntr + 1) line_breaks (chr = '\n')
  in
  get 0 0 0 [] false

let utf32_sub (str : string) (start : int) (length : int) =
  let finish = start + length in
  let rec sub str_pos cd_pos str_start str_finish =
    if str_pos = String.length str then
      String.sub str str_start (String.length str - str_start)
    else
      let str_start = if cd_pos = start then str_pos else str_start in
      if cd_pos = finish then String.sub str str_start (str_pos - str_start)
      else
        let utf8_length = utf8_length (String.unsafe_get str str_pos) in
        sub (str_pos + utf8_length) (cd_pos + 1) str_start str_finish
  in
  sub 0 0 0 0

(*
    The count_to functions duplicate some code intentionally.
    A previous generic version existed,
    but I was bothered by additional if-statements on each recursion
    to handle UTF-8/16 cases when the specified index is inside a code point.
*)
let count_to_utf32 (str : string) (count_to : int) =
  let rec cnt utf8_pos utf16_pos utf32_pos =
    if utf32_pos = count_to then create_offsets utf8_pos utf16_pos utf32_pos
    else
      let chr = String.unsafe_get str utf8_pos in
      let u8_length = utf8_length chr in
      let u16_length = utf16_length chr in

      let nextUtf8 = utf8_pos + u8_length in
      let nextUtf16 = utf16_pos + u16_length in
      let nextUtf32 = utf32_pos + 1 in
      cnt nextUtf8 nextUtf16 nextUtf32
  in
  cnt 0 0 0

let count_to_utf16 (str : string) (count_to : int) =
  let rec cnt utf8_pos utf16_pos utf32_pos =
    if utf16_pos = count_to then create_offsets utf8_pos utf16_pos utf32_pos
    else
      let chr = String.unsafe_get str utf8_pos in
      let u8_length = utf8_length chr in
      let u16_length = utf16_length chr in

      let next_u8 = utf8_pos + u8_length in
      let next_u16 = utf16_pos + u16_length in
      let next_u32 = utf32_pos + 1 in
      if next_u16 > count_to then create_offsets utf8_pos utf16_pos utf32_pos
      else cnt next_u8 next_u16 next_u32
  in
  cnt 0 0 0

let count_to_utf8 (str : string) (count_to : int) =
  let rec cnt utf8_pos utf16_pos utf32_pos =
    if utf8_pos = count_to then create_offsets utf8_pos utf16_pos utf32_pos
    else
      let chr = String.unsafe_get str utf8_pos in
      let u8_length = utf8_length chr in
      let u16_length = utf16_length chr in

      let next_u8 = utf8_pos + u8_length in
      let next_u16 = utf16_pos + u16_length in
      let next_u32 = utf32_pos + 1 in
      if next_u8 > count_to then create_offsets utf8_pos utf16_pos utf32_pos
      else cnt next_u8 next_u16 next_u32
  in
  cnt 0 0 0

let count_to (str : string) (count_towards : int) (enc : encoding) =
  match enc with
  | Utf8 -> count_to_utf8 str count_towards
  | Utf16 -> count_to_utf16 str count_towards
  | Utf32 -> count_to_utf32 str count_towards
