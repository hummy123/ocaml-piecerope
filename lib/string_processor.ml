(* This module contains Unicode-related functions. *)

(** Given the starting byte of a UTF-8 sequence, returns the character's length. *)
let char_bytes chr =
  match chr with
  | '\x00' .. '\x7f' -> 1
  | '\xc0' .. '\xdf' -> 2
  | '\xe0' .. '\xef' -> 3
  | '\xf0' .. '\xf7' -> 4
  | _ -> failwith "invalid utf-8 start"

(* Returns the integer representing this Unicode  *)
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

(** Returns the length of the string in terms of code points and the string position each line breaks occurs at. *)
let char_length_and_line_breaks (str: string) (pcStart: int) =
  let rec get strPos codepointCounter lineBreaks prevIsCr =
    if strPos >= String.length str then
      codepointCounter, lineBreaks |> List.rev |> Array.of_list
    else
      let charLegnth = char_bytes (String.unsafe_get str strPos) in
      let chr = String.unsafe_get str strPos in

      let lineBreaks =
        if chr = '\r' || (chr = '\n' && not prevIsCr) then
          (strPos + pcStart)::lineBreaks
        else
          lineBreaks
      in
      get (strPos + charLegnth) (codepointCounter + 1) lineBreaks (chr = '\n')
  in
  get 0 0 [] false

(** Gets a substring from a string with a codepoint inside it, in terms of code points and not UTF-8. *)
let codepointSub (str: string) (start: int) (length: int) tbl =
  let finish = start + length in
  let rec sub strPos cdPos strStart strFinish =
    if strPos = String.length str then
      String.sub str strStart (String.length str - strStart)
    else
      let charLegnth = char_bytes (String.unsafe_get str strPos) in
      if cdPos = start then
        sub (strPos + charLegnth) (cdPos + 1) strPos strFinish
      else if cdPos = finish then
        String.sub str strStart (strPos - strStart)
      else
        sub (strPos + charLegnth) (cdPos + 1) strStart strFinish
  in
  sub 0 0 0 0

