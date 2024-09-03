import * as cli from "jsr:@std/cli"

const params = cli.parseArgs(Deno.args)
const oldText = Deno.readTextFileSync(params.old)
const newText = Deno.readTextFileSync(params.new)

function str2entries(text: string): Map<string, string> {
  return text.split("\n").reduce((p, c) => {
    const [kana, rawHenkans] = c.substring(1).split(" ")
    if (!kana.startsWith(";") && rawHenkans) {
      p.set(kana, rawHenkans)
    }
    return p
  }, new Map<string, string>())
}

const oldEntries = str2entries(oldText)
const newEntries = str2entries(newText)

const allKeys = new Set([...oldEntries.keys(), ...newEntries.keys()])

const olds: string[] = []
const news: string[] = []

allKeys.forEach((v) => {
  const inOld = oldEntries.has(v)
  const inNew = newEntries.has(v)
  if (inOld && !inNew) {
    console.log(`\tRemove ${v} ${oldEntries.get(v)}`)
  } else if (!inOld && inNew) {
    console.log(`\tAdd ${v} ${newEntries.get(v)}`)
  } else {
    olds.push(`\tModify ${v} ${oldEntries.get(v)}`)
    news.push(`\tModify ${v} ${newEntries.get(v)}`)
  }
})

Deno.writeTextFileSync(params.old, olds.join("\n"))
Deno.writeTextFileSync(params.new, news.join("\n"))
