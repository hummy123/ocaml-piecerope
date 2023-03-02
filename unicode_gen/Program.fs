open System.IO

let getCategory (line: string) =
  let splitStr = line.Split("; ")
  let splitStr = splitStr[1].Split(" #")
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

(* Create string that will contain the category union. *)
let stringOfTypes = Map.fold (fun str cat _ -> str + " | " + cat) "type t =" map + " | Any"

(* Create string that will produce function to match int to cateory. *)
let patternMatchBase = @"
let intToCategory input =
  match input with
  "

let patternMatchString = 
  Map.fold (fun str cat arr ->
    let arr = Array.map (fun el -> el |> string) arr
    let codePoints = Array.fold (fun state el -> state + "| " + el + " ") "" arr
    str + codePoints + " -> " + cat + "\n  "
  ) patternMatchBase map
  + "| _ -> Any"

let output = stringOfTypes + "\n" + patternMatchString

File.WriteAllText("codepoint_converter.ml", output)

