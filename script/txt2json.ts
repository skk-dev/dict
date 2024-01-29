import P from "https://esm.sh/parsimmon@v1.18.1"
import YAML from "https://esm.sh/yaml@v2.1.1"
import iconv from "https://esm.sh/iconv-lite@0.6.3";
import * as cli from "https://deno.land/std@0.213.0/cli/mod.ts"

async function main(ifile: string, mfile: string, ofile: string) {
  function delisp(sexp: string): string {
    return sexp
      .replace(/\(concat "(.*?)"\)/g, "$1")
      .replace(/\\057/g, "/")
      .replace(/\\073/g, ";")
  }

  const LegacyFormat = P.createLanguage({
    comments: () => P.regexp(/^;;$|^;;.(?!okuri)[^\r\n]*?$/m).skip(P.newline).many(),
    kana: () => P.regexp(/^\S+/m),
    kanji: () => P.regexp(/([^\/\r\n;]+)(?:;[^\/\r\n;]+)*/, 1).map(delisp),
    record: Q => P.seq(Q.kana, P.string(' /').then(Q.kanji.skip(P.string('/')).many())),
    okuri_ari: Q => P.regexp(/^;;\s+okuri-ari entries.*?$/m).skip(P.newline).then(Q.record.skip(P.newline).many()),
    okuri_nasi: Q => P.regexp(/^;;\s+okuri-nasi entries.*?$/m).skip(P.newline).then(Q.record.skip(P.newline).many()),
    jisyo: Q => P.seqMap(
      Q.comments,
      Q.okuri_ari.map(Object.fromEntries),
      Q.okuri_nasi.map(Object.fromEntries),
      (_, okuri_ari, okuri_nasi) => ({ okuri_ari, okuri_nasi })
    )
  })
  const input = await Deno.readFile(ifile)
  const text = iconv.decode(input, "EUC-JP")
  const meta = YAML.parse(await Deno.readTextFile(mfile))
  const jisyo = {
    ...meta,
    ...(LegacyFormat.jisyo.parse(text) as any).value
  }
  await Deno.writeTextFile(ofile, JSON.stringify(jisyo, null, 2))
}


const params = cli.parseArgs(Deno.args)
await main(params.i, params.m, params.o)