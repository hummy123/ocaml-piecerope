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

