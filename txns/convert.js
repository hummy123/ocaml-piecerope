const fs = require("fs")
const path = require("path")

const filename = "sephblog"

const inputPath = path.resolve(`./${filename}.json`)

const {
  txns
} = JSON.parse(fs.readFileSync(inputPath, 'utf-8'))

// Counter to stop applying transactinos after a certainn nmber of steps (useful for debugging)
let counter = 0
const COUNTER_STOP = 999999999

// Generate array of tuples that can be executed in code.
let data = `
let data = [|
`

for (const entry of txns) {
    if (counter < COUNTER_STOP) {
        for (const [pos, delNum, insStr] of entry.patches) {
          const str = JSON.stringify(insStr) // escape special chars in string
        	data += `  (${pos}, ${delNum}, ${str});\n`
        }
        counter += 1
    }
}

data += `
  |]
`

let dataOutputPath = `${filename}.ml`

fs.writeFileSync(dataOutputPath, data)

// Reset counter and generate target string using counter.
counter = 0
let content = ""

for (const top of txns) {
    if (counter < COUNTER_STOP) {
        for (const [pos, delHere, insContent] of top.patches) {
        const before = content.slice(0, pos)
        const after = content.slice(pos + delHere)
        content = before + insContent + after
        }
        counter += 1
    }
}

content = "let str = " + JSON.stringify(content)

let contentOutputPath = `${filename}string.ml`

fs.writeFileSync(contentOutputPath, content)

