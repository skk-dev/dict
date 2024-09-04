import * as cli from "jsr:@std/cli"
import iconv from "https://esm.sh/iconv-lite@0.6.3"

{
  const params = cli.parseArgs(Deno.args)
  const exitcode = main(params.c, params.i, params.o)
  Deno.exit(exitcode)
}

function main(
  coding: string,
  ifile: string,
  ofile: string,
): number {
  if (!ifile || !ofile) {
    console.error("options: [-c charset] -i input -o skkdic")
    return 1
  }

  const input = Deno.readFileSync(ifile)
  const text = coding
    ? iconv.decode(input, coding)
    : new TextDecoder().decode(input)

  const ejMap = new Map<string, string[]>()
  text.split("\n").forEach((line, index) => {
    const [former, latter] = line.split(" /", 2)
    if (!latter) return
    if (index == 0) {
      console.info(
        latter.split(/, |\//).filter((v) => v),
      )
      return
    }
    const [textKanjis, _yomis] = former.split(" ", 2)
    const kanjis = textKanjis
      .split(";")
      .map((v) => v.replace(/\(.+?\)$/, ""))
    let unfinished = "" // 括弧内にスラッシュがある場合
    const words = latter
      .replace(/\/EntL[^\/]+\/$/, "")
      .split("/")
      .map((v) => {
        const word = unfinished ? `${unfinished}/${v}` : v
        unfinished = ""
        if (
          Number(word.match(/\(/g)?.length) >
            Number(word.match(/\)/g)?.length)
        ) {
          unfinished = word
          return ""
        }
        return word
      })
      .reduce((p, c) => {
        const word = c
          .replace(/^(\([^\)]+\)+ +)*/, "")
          .replace(/^({[^}]+}+ +)*/, "")
        const variation: string[] = []
        let matches
        if ((matches = word.match(/^\(([^ \)]+)\)([^ ]+)$/))) {
          // (self-)complacency
          variation.push(matches[1] + matches[2]) // self-complacency
          variation.push(matches[2]) // complacency
        } else {
          variation.push(word)
        }
        variation.forEach((v) => {
          if ((matches = word.match(/^([^ ]+)\(([^ \)]+)\)$/))) {
            // cover(ing)
            p.push(matches[1] + matches[2]) // covering
            p.push(matches[1]) // cover
          } else {
            p.push(v)
          }
        })
        return p
      }, [""])
      .map((v) => v?.replace('"', ""))
      .filter((v) => v && v != "(P)" && !/ |[^!-~]/.test(v))
    if (kanjis.length && words.length) {
      words.forEach((en) => {
        if (!en?.length) return
        const oldKanjis = ejMap.get(en)
        const newKanjis = oldKanjis ? oldKanjis : []
        newKanjis.push(...kanjis)
        ejMap.set(en, newKanjis)
      })
    }
  })

  const jisyo = ";; okuri-ari entries.\n;; okuri-nasi entries.\n" +
    [...ejMap.keys()]
      .sort()
      .map((en) => {
        const kanjis = ejMap.get(en)
        if (!kanjis) return
        const text = [...new Set(kanjis)].map((v) => enlisp(v)).join("/")
        return `${en} /${text}/`
      })
      .join("\n") +
    "\n"
  Deno.writeTextFileSync(ofile, jisyo)
  return 0
}

function enlisp(s: string | undefined): string {
  if (!s) return ""
  return /[\/;]/.test(s)
    ? `(concat "${s.replace(/\//g, "\\057").replace(/;/g, "\\073")}")`
    : s
}
