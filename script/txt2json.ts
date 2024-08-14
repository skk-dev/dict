import P from "https://esm.sh/parsimmon@v1.18.1"
import YAML from "https://esm.sh/yaml@v2.1.1"
import iconv from "https://esm.sh/iconv-lite@0.6.3"
import * as cli from "https://deno.land/std@0.213.0/cli/mod.ts"

async function main(
  coding: string,
  ifile: string,
  mfile: string,
  ofile: string,
): Promise<number> {
  function uncomment(sexp: string): string {
    return sexp
      .replace(/^;; */g, "")
  }
  function delisp(sexp: string): string {
    return sexp
      .replace(/\(concat "(.*?)"\)/g, "$1")
      .replace(/\\057/g, "/")
      .replace(/\\073/g, ";")
      .replace(/\x00\x00\x01\x00\x00/, "﨑")
      .replace(/\x00\x00\x02\x00\x00/, "琇")
      .replace(/\x00\x00\x03\x00\x00/, "璉")
      .replace(/\x00\x00\x04\x00\x00/, "侊")
      .replace(/\x00\x00\x05\x00\x00/, "錥")
      .replace(/\x00\x00\x06\x00\x00/, "昉")
      .replace(/\x00\x00\x07\x00\x00/, "枻")
      .replace(/\x00\x00\x08\x00\x00/, "彅")
      .replace(/\x00\x00\x09\x00\x00/, "桒")
      .replace(/\x00\x00\x00\x01\x00/, "彧")
      .replace(/\x00\x00\x00\x02\x00/, "涬")
      .replace(/\x00\x00\x00\x03\x00/, "曻")
      .replace(/\x00\x00\x00\x04\x00/, "杦")
      .replace(/\x00\x00\x00\x05\x00/, "銈")
      .replace(/\x00\x00\x00\x06\x00/, "暲")
      .replace(/\x00\x00\x00\x07\x00/, "顥")
      .replace(/\x00\x00\x00\x08\x00/, "嵂") // なかむらかつおさんは「葎」が正しいのでは？
      .replace(/\x00\x00\x00\x09\x00/, "葎")
      .replace(/\x00\x00\x00\x00\x01/, "橳")
      .replace(/\x00\x00\x00\x00\x02/, "侚")
      .replace(/\x00\x00\x00\x00\x03/, "尞")
      .replace(/\x00\x00\x00\x00\x04/, "愰")
      .replace(/\x00\x00\x00\x00\x05/, "兊")
      .replace(/\x00\x00\x00\x00\x06/, "畯")
      .replace(/\x00\x00\x00\x00\x07/, "燾")
      .replace(/\x00\x00\x00\x00\x08/, "栁")
      .replace(/\x00\x00\x00\x00\x09/, "燁")
      .replace(/\x00\x00\x01\x01\x00/, "竑")
      .replace(/\x00\x00\x01\x02\x00/, "煇")
      .replace(/\x00\x00\x01\x03\x00/, "訷")
      .replace(/\x00\x00\x01\x04\x00/, "蕙")
  }
  function split_annotations(sexp: string): string[] {
    return sexp
      .split(";")
      .filter((v) => v.length > 0)
      .map((v) => v
        .replace("㈱", "(株)")
        .replace("Ⅳ", "IV")
      )
  }
  function markJISX0213(s: Uint8Array): Uint8Array {
    let deleteCount = 0
    const b = s.reduce((p, v, i, a) => {
      if (deleteCount > 0) {
        deleteCount--
      } else if ( // s/CFF2/FEA891/g
        a.length > i + 2 &&
        a[i+0] == 0xCF &&
        a[i+1] == 0xF2
      ) {
        p.push(0x00, 0x00, 0x01, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF8 &&
        a[i+1] == 0xA1
      ) {
        p.push(0x00, 0x00, 0x02, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF8 &&
        a[i+1] == 0xB8
      ) {
        p.push(0x00, 0x00, 0x03, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xAE &&
        a[i+1] == 0xB5
      ) {
        p.push(0x00, 0x00, 0x04, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xFD &&
        a[i+1] == 0xB8
      ) {
        p.push(0x00, 0x00, 0x05, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF5 &&
        a[i+1] == 0xAD
      ) {
        p.push(0x00, 0x00, 0x06, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF5 &&
        a[i+1] == 0xD8
      ) {
        p.push(0x00, 0x00, 0x07, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF4 &&
        a[i+1] == 0xBA
      ) {
        p.push(0x00, 0x00, 0x08, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF5 &&
        a[i+1] == 0xDE
      ) {
        p.push(0x00, 0x00, 0x09, 0x00, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF4 &&
        a[i+1] == 0xBE
      ) {
        p.push(0x00, 0x00, 0x00, 0x01, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF6 &&
        a[i+1] == 0xEF
      ) {
        p.push(0x00, 0x00, 0x00, 0x02, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF5 &&
        a[i+1] == 0xB7
      ) {
        p.push(0x00, 0x00, 0x00, 0x03, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF5 &&
        a[i+1] == 0xD0
      ) {
        p.push(0x00, 0x00, 0x00, 0x04, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xFD &&
        a[i+1] == 0xAE
      ) {
        p.push(0x00, 0x00, 0x00, 0x05, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF5 &&
        a[i+1] == 0xC5
      ) {
        p.push(0x00, 0x00, 0x00, 0x06, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xFE &&
        a[i+1] == 0xA5
      ) {
        p.push(0x00, 0x00, 0x00, 0x07, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 3 &&
        a[i+0] == 0x8F &&
        a[i+1] == 0xA8 &&
        a[i+2] == 0xD5
      ) {
        p.push(0x00, 0x00, 0x00, 0x08, 0x00)
        deleteCount = 2
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xCE &&
        a[i+1] == 0xAA
      ) {
        p.push(0x00, 0x00, 0x00, 0x09, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF6 &&
        a[i+1] == 0xB1
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x01)
        deleteCount = 1
      } else if (
        a.length > i + 3 &&
        a[i+0] == 0x8F &&
        a[i+1] == 0xA1 &&
        a[i+2] == 0xCE
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x02)
        deleteCount = 2
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xCF &&
        a[i+1] == 0xDC
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x03)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF4 &&
        a[i+1] == 0xDA
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x04)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xAE &&
        a[i+1] == 0xCF
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x05)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF8 &&
        a[i+1] == 0xCA
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x06)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF7 &&
        a[i+1] == 0xE1
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x07)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF5 &&
        a[i+1] == 0xDD
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x08)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF7 &&
        a[i+1] == 0xDE
      ) {
        p.push(0x00, 0x00, 0x00, 0x00, 0x09)
        deleteCount = 1
      } else if (
        a.length > i + 3 &&
        a[i+0] == 0x8F &&
        a[i+1] == 0xF3 &&
        a[i+2] == 0xB9
      ) {
        p.push(0x00, 0x00, 0x01, 0x01, 0x00)
        deleteCount = 2
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xF7 &&
        a[i+1] == 0xD3
      ) {
        p.push(0x00, 0x00, 0x01, 0x02, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xFC &&
        a[i+1] == 0xA2
      ) {
        p.push(0x00, 0x00, 0x01, 0x03, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 2 &&
        a[i+0] == 0xFB &&
        a[i+1] == 0xB8
      ) {
        p.push(0x00, 0x00, 0x01, 0x04, 0x00)
        deleteCount = 1
      } else if (
        a.length > i + 3 &&
        a[i] == 0x8F
      ) {
        p.push(a[i], a[i+1], a[i+2])
        deleteCount = 2
      } else if (
        a.length > i + 2 &&
        a[i] > 0xA0
      ) {
        p.push(a[i], a[i+1])
        deleteCount = 1
      } else {
        p.push(v)
      }
      return p
    }, new Array<number>())
    return Uint8Array.from(b)
  }

  const LegacyFormat = P.createLanguage<
    SKKJisyo & {
      record: Entry
      kana: Kana
      kanji: Kanji
      jisyo: SKKJisyo
    }
  >({
    comments: () => P.regexp(/^;;$|^;;.(?!okuri)[^\r\n]*?$/m).map(uncomment).skip(P.newline).many(),
    kana: () => P.regexp(/^\S+/m),
    kanji: () =>
      P.seq(
        P.regexp(/[^\/\r\n;]+/).map(delisp),
        P.regexp(/(;[^\/\r\n;]+)*/).map(split_annotations),
      ),
    record: (Q) => P.seq(Q.kana, P.string(" /").then(Q.kanji.skip(P.string("/")).many())),
    okuri_ari: (Q) =>
      P.regexp(/^;;\s+okuri-ari entries.*?$/m).skip(P.newline).then(
        Q.record.skip(P.newline).many(),
      ),
    okuri_nasi: (Q) =>
      P.regexp(/^;;\s+okuri-nasi entries.*?$/m).skip(P.newline).then(
        Q.record.skip(P.newline).many(),
      ),
    jisyo: (Q) =>
      P.seqMap(
        Q.comments,
        Q.okuri_ari,
        Q.okuri_nasi,
        (comments, okuri_ari, okuri_nasi) => ({ comments, okuri_ari, okuri_nasi }),
      ),
  })
  const input = markJISX0213(await Deno.readFile(ifile))
  const text = iconv.decode(input, coding)
  const result = LegacyFormat.jisyo.parse(text)
  if (!result.status) return result.index.line
  const meta = YAML.parse(await Deno.readTextFile(mfile))
  const jisyo = result.value
  const json = {
    ...meta,
    okuri_ari: jisyo.okuri_ari,
    okuri_nasi: jisyo.okuri_nasi,
  }
  const output = JSON.stringify(json, null, 2)
    .replaceAll(/\r?\n {10,}/g, " ")
    .replaceAll(/\r?\n {8}\]/g, " ]")
    .replaceAll(/\r?\n {6}([["])/g, " $1")
    .replaceAll(/\r?\n {6}\](\r?\n) {4}\]/g, "$1    ] ]")
    .replaceAll(/ {8}/g, " ".repeat(6))
  await Deno.writeTextFile(ofile, output)
  return 0
}

const params = cli.parseArgs(Deno.args)
await main(params.c, params.i, params.m, params.o)
