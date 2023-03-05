(* Functions for counting code points and line breaks. *)
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

(* Functions for clipping to start of code point. *)
let is_start chr =
  match chr with
  | '\x00' .. '\xf7' -> true
  | _ -> false

let rec clip_to_start idx str =
  if is_start (String.unsafe_get str idx) || idx = 0 then
    idx
  else
    clip_to_start (idx - 1) str

