const fs = require("fs")
const path = require("path")

const filename = "rustcode"

const inputPath = path.resolve(`./${filename}.json`)

const {
  txns
} = JSON.parse(fs.readFileSync(inputPath, 'utf-8'))

let content = `
open Piecerope

let lst = [|
`

for (const entry of txns) {
    for (const [pos, delNum, insStr] of entry.patches) {
    	content += `  (${pos}, ${delNum}, "${escape(insStr)}");\n`
    }
}

content += `
  |]
  
let run() =
  Printf.printf "\nStarting ${filename}...";
  let t = Sys.time() in
  let _ = Array.fold_left (fun acc (pos, delNum, insStr) ->
    let rope = 
      if delNum > 0 then
        Piece_rope.delete pos delNum acc
      else
        acc
    in
    let rope =
      if insStr <> String.empty then
        Piece_rope.insert pos insStr rope
      else
        rope
    in
    rope) Piece_rope.empty lst in
  let endTime = (Sys.time() -. t) *. 1000.0 in
  Printf.printf "Execution time: %f ms\n" endTime ;
`

let outputPath = `${filename}.ml`

fs.writeFileSync(outputPath, content)
