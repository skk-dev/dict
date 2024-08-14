import iconv from "https://esm.sh/iconv-lite@0.6.3"
import * as cli from "https://deno.land/std@0.213.0/cli/mod.ts"

async function main(coding: string, ifile: string, ofile: string) {
  function enlisp(sexp: string): string {
    const first = sexp.search(/[/;]|\\n/)
    return first < 0 ? sexp : `(concat "${
      sexp
        .replace(/\//g, "\\057")
        .replace(/;/g, "\\073")
    }")`
  }
  function reduceRecords(records: Entry[]): string {
    return records.reduce((acc: string, rec: Entry): string => {
      return acc + rec[0] + " /" +
        rec[1].reduce((ac: string, kanji: Kanji): string => {
          return ac + enlisp(kanji[0]) +
            kanji[1].reduce((a: string, s: string) => {
              return a + ";" + s
            }, "") + "/"
        }, "") + "\n"
    }, "")
  }
  function replaceWrongChars(s: Uint8Array): Uint8Array {
    if (coding != "EUC-JP") return s
    let deleteCount = 0
    const b = s.reduce((p, v, i, a) => {
      if (deleteCount > 0) {
        deleteCount--
      } else if ( // s/8FA2B7/A1C1/g
        a.length > i + 3 &&
        a[i+0] == 0x8F &&
        a[i+1] == 0xA2 &&
        a[i+2] == 0xB7
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
  const jisyo: SKKJisyo = {
    comments: [
      json.description,
      json.copyright,
      json.license,
      json.note,
    ],
    okuri_ari: json.okuri_ari,
    okuri_nasi: json.okuri_nasi,
  }
  jisyo.toString = () =>
    `-*- mode: fundamental; coding: ${coding.toLowerCase()} -*-\n` +
    jisyo.comments.reduce((acc: string, com: string): string => {
      if (typeof com !== "string") return acc + ";;\n"
      return acc + com.split("\n").reduce((a: string, l: string): string => {
        return a + `;; ${l}\n`
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
