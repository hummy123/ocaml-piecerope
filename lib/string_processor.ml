(* Functions for counting code points and line breaks. *)
let char_bytes chr =
  match chr with
  | '\x00' .. '\x7f' -> 1
  | '\xc0' .. '\xdf' -> 2
  | '\xe0' .. '\xef' -> 3
  | '\xf0' .. '\xf7' -> 4
  | _ -> failwith "invalid utf-8 start"

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

(* Gets a substring from a string with a codepoint inside it.*)
let codepointSub (str: string) (start: int) (length: int) =
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
        let charLegnth = char_bytes (String.unsafe_get str strPos) in
        sub (strPos + charLegnth) (cdPos + 1) strStart strFinish
  in
  sub 0 0 0 0

