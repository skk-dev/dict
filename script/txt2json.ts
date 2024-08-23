import P from "https://esm.sh/parsimmon@v1.18.1"
import * as YAML from "jsr:@std/yaml"
import iconv from "https://esm.sh/iconv-lite@0.6.3"
import * as cli from "jsr:@std/cli"
import Ajv from "https://esm.sh/ajv@8.17.1"

async function main(
  coding: string,
  ifile: string,
  mfile: string,
  ofile: string,
  sfile: string,
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
      record: SKKEntry
      kana: string
      henkan: SKKHenkan
      jisyo: SKKJisyo
    }
  >({
    meta: () =>
      P.seqMap(
        P.regexp(/^;;$|^;;.(?!okuri)[^\r\n]*?$/m).map(uncomment).skip(P.newline).many(),
        (lines) => ({ // 今のところ不要なのでパースしていない
          description: "",
          copyright: "",
          license: "",
          note: lines.join("\n"),
        }),
      ),
    kana: () => P.regexp(/^\S+/m),
    henkan: () =>
      P.seqMap(
        P.regexp(/[^\/\r\n;]+/).map(delisp),
        P.regexp(/(;[^\/\r\n;]+)*/).map(splitAnnotations),
        (kanji, annotations) => ({ [kanji]: annotations }),
      ),
    record: (Q) =>
      P.seqMap(
        Q.kana,
        P.string(" /").then(Q.henkan.skip(P.string("/")).many()),
        (kana, henkans) => ({ [kana]: henkans }),
      ),
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
        Q.meta,
        Q.okuri_ari,
        Q.okuri_nasi,
        (meta, okuri_ari, okuri_nasi) => ({ meta, okuri_ari, okuri_nasi }),
      ),
  })
  const input = await Deno.readFile(ifile)
  const text = iconv.decode(input, coding)
  const result = LegacyFormat.jisyo.parse(text)
  if (!result.status) return result.index.line
  const meta = YAML.parse(await Deno.readTextFile(mfile)) as SKKMeta
  const jisyo = result.value
  const json = {
    ...meta,
    version: "0.1.0",
    okuri_ari: jisyo.okuri_ari,
    okuri_nasi: jisyo.okuri_nasi,
  }
  const output = JSON.stringify(json, null, 2)
    .replaceAll(/\r?\n {8,}(["}])/g, " $1")
  await Deno.writeTextFile(ofile, output)

  if (!sfile) return 0
  if (await validate(ofile, sfile)) return 0
  return 1
}

async function validate(ofile: string, sfile: string): Promise<boolean> {
  const jsonText = await Deno.readTextFile(ofile)
  const json = JSON.parse(jsonText)
  const schemeText = await Deno.readTextFile(sfile)
  const schema = JSON.parse(schemeText)

  const ajv = new Ajv()
  const validate = ajv.compile(schema)
  if (validate(json)) return true
  console.error(validate.errors)
  return false
}

const params = cli.parseArgs(Deno.args)
Deno.exit(await main(params.c, params.i, params.m, params.o, params.s))
