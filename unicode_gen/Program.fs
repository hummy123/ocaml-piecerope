open System.IO

let getCategory (line: string) =
  let splitStr = line.Split("; ")
  let splitStr = splitStr[1].Split(" #")
  splitStr[0]

let getExPictoCategory (line: string) =
  let splitStr = line.Split("; ")
  let splitStr = splitStr[1].Split("#")
  splitStr[0]

let getLineCodes (line: string) =
  let line = line.Split(" ")[0] (* Get code..point range, *)
  if line.Contains("..") then
    let split = line.Split("..")
    let first = "0x" + split[0] |> int
    let last = "0x" + split[1] |> int
    [|first..last|]
  else
    let point = "0x" + line |> int
    [|point|]

let lines = File.ReadAllLines "./GraphemeBreakProperty.txt"

(* Map to store grapheme categories, without duplicates. *)
let mutable map: Map<string, int array> = Map.empty

(* First step is to extract the categories and convert them into a string that can contain a discriminated union. *)
for line in lines do
  (* There is a code point on this line. *)
  if not (line.StartsWith "#" || line = "") then
    let category = getCategory line
    let lineCodes = getLineCodes line
    match map.TryFind category with
    | Some value -> 
        let newArr = Array.append value lineCodes
        map <- map.Add(category, newArr)
    | None ->
        map <- map.Add(category, lineCodes)

(* We also want the Extended_Pictographic property from emoji-data.txt . *)
let emojiLines = File.ReadAllLines "./emoji-data.txt"

for line in emojiLines do
  if not (line.StartsWith "#" || line = "") then
    let category = getExPictoCategory line
    if category = "Extended_Pictographic" then
      let lineCodes = getLineCodes line
      match map.TryFind category with
      | Some value -> 
          let newArr = Array.append value lineCodes
          map <- map.Add(category, newArr)
      | None ->
          map <- map.Add(category, lineCodes)

(* Create string that will contain the category union. *)
let stringOfTypes = Map.fold (fun str cat _ -> str + " | " + cat) "type t =" map + " | Any"

(* Create string that will produce hash table to match int to category. *)
let totalLength: int = Map.fold (fun acc _ (el: int array) -> acc + el.Length) 0 map
let hashTableStringBase = "let tbl = Hashtbl.create " + string totalLength  + ";;\n"
let hashTableString = 
  Map.fold (fun str cat (arr: int array) ->
    let arr = Array.map (fun el -> el |> string) arr (* Manipulate ints as strings in generator *)

    let codePoints = Array.fold (fun state el -> state + "Hashtbl.add tbl (" + el + ", " + cat + ");;\n") "" arr

    str + codePoints 
  ) hashTableStringBase map

(* String containing function to get category from string. *)
let intToCategoryString = @"
let intToCategory x =
  match Hashtbl.find_opt x with
  | Some(_, cat) -> cat
  | None -> Any
"

let output = stringOfTypes + "\n\n" + hashTableString + "\n" + intToCategoryString

File.WriteAllText("codepoint_converter.ml", output)

let mli = stringOfTypes + "\n" + @"(** Convert an integer to a Unicode codepoint category. *)
val intToCategory : int -> t"
File.WriteAllText("codepoint_converter.mli", mli)

