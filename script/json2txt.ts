import iconv from "https://esm.sh/iconv-lite@0.6.3"
import * as cli from "jsr:@std/cli"

async function main(coding: string, ifile: string, ofile: string) {
  function enlisp(sexp: string): string {
    const first = sexp.search(/[/;]|\\n/)
    return first < 0 ? sexp : `(concat "${
      sexp
        .replace(/\//g, "\\057")
        .replace(/;/g, "\\073")
    }")`
  }
  function reduceRecords(records: SKKEntry[]): string {
    return records.reduce((acc: string, ent: SKKEntry): string => {
      const kana = Object.keys(ent)[0]
      return acc + kana + " /" +
        ent[kana].reduce((ac: string, henkan: SKKHenkan): string => {
          const kanji = Object.keys(henkan)[0]
          return ac + enlisp(kanji) +
            henkan[kanji].reduce((a: string, s: string) => {
              return a + ";" + s
            }, "") + "/"
        }, "") + "\n"
    }, "")
  }
  function replaceWrongChars(s: Uint8Array): Uint8Array {
    if (!/^euc-?j/i.test(coding)) return s
    let deleteCount = 0
    const b = s.reduce((p, v, i, a) => {
      if (deleteCount > 0) {
        deleteCount--
      } else if (
        // s/8FA2B7/A1C1/g
        a.length > i + 3 &&
        a[i + 0] == 0x8F &&
        a[i + 1] == 0xA2 &&
        a[i + 2] == 0xB7
      ) {
        p.push(0xA1, 0xC1)
        deleteCount = 2
      } else {
        p.push(v)
      }
      return p
    }, new Array<number>())
    return Uint8Array.from(b)
  }

  const input = await Deno.readTextFile(ifile)
  const json = JSON.parse(input)
  const jisyo: SKKJisyo = json // meta が欠落している
  const meta: SKKMeta = json
  jisyo.toString = () =>
    `;; -*- mode: fundamental; coding: ${coding.toLowerCase()} -*-\n` +
    Object.entries(meta).reduce((acc, com): string => {
      const key = com[0]
      const value = com[1]
      if (value == undefined) return acc + ";;\n"
      if (key == "version" || typeof value !== "string") return acc
      return acc + value.split("\n").reduce((a: string, l: string): string => {
        return a + `;;${l.length ? " " : ""}${l}\n`
      }, "") + ";;\n"
    }, "") +
    ";; okuri-ari entries.\n" +
    reduceRecords(jisyo.okuri_ari) +
    ";; okuri-nasi entries.\n" +
    reduceRecords(jisyo.okuri_nasi)

  const output = replaceWrongChars(iconv.encode(jisyo.toString(), coding))
  await Deno.writeFile(ofile, output)
}

const params = cli.parseArgs(Deno.args)
await main(params.c, params.i, params.o)
