const fs = require("fs")
const path = require("path")

const filename = "automerge_paper"

const inputPath = path.resolve(`./${filename}.json`)

const {
  txns
} = JSON.parse(fs.readFileSync(inputPath, 'utf-8'))

let content = `
let ig _ = ()

let run() =
  let t = Sys.time() in
  let r = Piecerope.empty in
`

for (const entry of txns) {
    for (const [pos, delNum, insStr] of entry.patches) {
        if (delNum > 0) {
            content += `  let r = Piecerope.delete ${pos} ${delNum} r in\n`
        }
        if (insStr !== "") {
            content += `  let r = Piecerope.insert ${pos} \"${escape(insStr)}\" r in\n`
        }
    }
}

content += `

  let endTime = Sys.time() -. t in
  Printf.printf "Execution time: %fs" endTime ;
  ig r

`

let outputPath = `${filename}.ml`

fs.writeFileSync(outputPath, content)
