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

(* Create string that will contain the category union and count total code points. *)
let (typeString, totalPoints) = Map.fold (fun (str, count) cat (arr: int array) -> 
  str + " | " + cat, count + arr.Length) ("type t =", 0) map // + " | Any"

let stringOfTypes = typeString + " | Any"

(* File containing categories and its corresponding mli. *)
File.WriteAllText("../lib/codepoint_types.ml", stringOfTypes)

File.WriteAllText("../lib/codepoint_types.mli", stringOfTypes)

(* Next step is to insert code points into map. *)
let mapStringBase = "let tbl: (int, Codepoint_types.t) Hashtbl.t  = Hashtbl.create " + string totalPoints + " in\n"

let mapString = 
  Map.fold (fun str cat arr -> 
    Array.fold (fun state el ->
      let el = string el
      state + "let _ = Hashtbl.add tbl " + el + " " + cat + " in\n"
    ) str arr
  ) mapStringBase map 
  + "\ntbl"

let hashtblString = "let hashTableGen _ =\n" + mapString

File.WriteAllText("../lib/codepoint_values.ml", hashtblString)

