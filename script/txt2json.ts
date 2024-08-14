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
  }
  function splitAnnotations(sexp: string): string[] {
    return sexp
      .split(";")
      .filter((v) => v.length > 0)
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
        P.regexp(/(;[^\/\r\n;]+)*/).map(splitAnnotations),
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
  const input = await Deno.readFile(ifile)
  const text = iconv.decode(input, coding)
  const result = LegacyFormat.jisyo.parse(text)
  if (!result.status) return result.index.line
  const meta = YAML.parse(await Deno.readTextFile(mfile))
  const jisyo = result.value
  const json = {
    ...meta,
    version: "0.1.0",
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
