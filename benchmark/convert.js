const fs = require("fs")
const path = require("path")

const filename = "sephblog"

const inputPath = path.resolve(`./${filename}.json`)

const {
  txns
} = JSON.parse(fs.readFileSync(inputPath, 'utf-8'))

let content = `
let data = [|
`

for (const entry of txns) {
    for (const [pos, delNum, insStr] of entry.patches) {
    	content += `  (${pos}, ${delNum}, "${escape(insStr)}");\n`
    }
}

content += `
  |]
`

let outputPath = `${filename}.ml`

fs.writeFileSync(outputPath, content)
