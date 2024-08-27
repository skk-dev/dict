import * as cli from "jsr:@std/cli"
import * as XML from "jsr:@libs/xml"
import iconv from "https://esm.sh/iconv-lite@0.6.3"

{
  const params = cli.parseArgs(Deno.args)
  const exitcode = main(params.c, params.i, params.o, params.p, params.x)
  Deno.exit(exitcode)
}

function main(
  coding: string,
  ifile: string,
  ofile: string,
  pfile: string,
  xfile: string,
): number {
  if (!ofile || !xfile) {
    console.error([
      "options: -o skkdic -x xml [[-c charset] -i input] [-p predef]",
      "",
      "converts -x to -o, transliterating kanjis",
      "  using -i SKK dict and -p SKK-like file.",
      "ignores -c -i and -p when -x is not ja.xml.",
      "prefers -p to -i when transliterating.",
      "outputs to -o in a format for -p when -o and -p are the same.",
      "  outputs normal SKK dict format otherwise.",
      "reads -i as -c if specified.",
      "  assumes -i is in UTF-8 otherwise.",
      "",
      "example usage:",
      "  touch SKK-JISYO.emoji.predef",
      "  deno emoji.ts -o SKK-JISYO.emoji.predef -x ja.xml \\",
      "    -c EUC-JP -i SKK-JISYO.L+ -p SKK-JISYO.emoji.predef",
      "  $EDITOR SKK-JISYO.emoji.predef",
      "  deno emoji.ts -o SKK-JISYO.emoji.ja -x ja.xml \\",
      "    -p SKK-JISYO.emoji.predef",
      "  deno emoji.ts -o SKK-JISYO.emoji.en -x en.xml",
      "  skkdic-expr2 SKK-JISYO.emoji.en + SKK-JISYO.emoji.ja \\",
      "    > SKK-JISYO.emoji",
    ].join("\n"))
    return 1
  }
  const isJaXML = xfile.endsWith("ja.xml")
  if (!isJaXML && (pfile || ifile)) {
    console.warn("-i and -p are ignored when -x is not ja.xml")
  }
  if (!ifile && coding) {
    console.warn("-c is ignored when -i is not given")
  }

  const revMap = new Map<string, string[]>() // 逆変換
  if (isJaXML && ifile) {
    const input = Deno.readFileSync(ifile)
    const text = coding
      ? iconv.decode(input, coding)
      : new TextDecoder().decode(input)

    let okuri_nasi = false
    text.split("\n").forEach((line) => {
      if (/^;;\s+okuri-nasi entries./.test(line)) {
        okuri_nasi = true
        return
      } else if (okuri_nasi && !line.startsWith(";")) {
        const [kana, henkans] = line.split(" /", 2)
        if (kana && !kana.includes(">") && henkans) {
          henkans.split("/").forEach((henkanWithAnnotation) => {
            const henkan = henkanWithAnnotation.split(";")[0]
            const kanas = revMap.get(henkan)
            kanas?.push(kana)
            revMap.set(henkan, kanas ? kanas : [kana])
          })
        }
      }
    })
  }

  const cpTexts = extractFromXml(xfile) // 絵文字と漢字ObjectのSet
  const oldRevs = (isJaXML && pfile)
    ? oldKanasInEmoJisyo(pfile) // 漢字かなMap
    : new Map<string, string>()
  const emoJisyo = ";; okuri-ari entries.\n;; okuri-nasi entries.\n" +
    [...cpTexts.values()]
      .sort((a, b) => a.text.localeCompare(b.text))
      .map((cpText, index) => {
        let isComment = false
        let kana: string | undefined = cpText.text
        const others = cpText.all.length > 1
          ? `, (${cpText.all.filter((v) => v != kana).join(", ")})`
          : ""

        if (isJaXML) {
          const text = cpText.text.replace(" ", "") // ※と一致させること
          const oldKana = oldRevs.get(text)
          const revKana = revMap.get(text)?.join("/")
          if (/\p{Script_Extensions=Han}/u.test(kana)) { // 漢字を含む
            kana = oldKana ? oldKana : revKana // undefined の場合もある
          } else {
            const notKana =
              /[^\p{Script_Extensions=Hiragana}\p{Script_Extensions=Katakana}]/u
            if (notKana.test(kana) || notKana.test(text)) {
              kana = oldKana
                ? oldKana
                : revKana
                ? revKana
                : (isComment = true) && kana
            }
          }
        } else {
          const normalKana = kana.normalize("NFKD") // アクセント記号など分離
            // .replace(" ", "_") // スペースを変換するか消すか、好みの問題
            .replace(/[^a-zA-Z0-9!\?\*\+-]/g, "") // ホワイトリスト
            .toLowerCase() // 最終的にスペースも大文字もない状態
          isComment = !normalKana
          kana = normalKana ? normalKana : kana
        }
        const enlispedCp = enlisp(cpText.cp)
        const codeOfCp = enlisp(
          [...cpText.cp]
            .map((c) => `U+${c.codePointAt(0)?.toString(16)}`)
            .join(","), // ", " ではない
        )
        const annotation = enlisp(
          (pfile == ofile)
            ? `${cpText.text}${others}` // -p ファイル作成用
            : cpText.all.toString(), // skkdic-expr2 にかけて合成する用
        )
        const outputLine = `${isComment ? "; " : ""}${
          hiragana(kana)
        } /${enlispedCp};${codeOfCp};${annotation}/`
        if (!isComment && (kana == undefined || kana.includes("/"))) {
          console.info(index, outputLine) // 更新すべき項目
        }
        return outputLine
      })
      .join("\n")
  Deno.writeTextFileSync(ofile, emoJisyo)
  return 0
}

type cp_text = { cp: string; text: string; all: string[] }
function extractFromXml(xfile: string): Set<cp_text> {
  using xmlFile = Deno.openSync(xfile)
  const xml = XML.parse(xmlFile)
  const ldml = xml["ldml"] as XML.xml_node
  const annotations = ldml["annotations"] as XML.xml_node
  const anns = annotations["~children"] as XML.xml_node[]
  const jas = new Set<cp_text>()
  anns.forEach((anno) => {
    if (anno["~name"] != "annotation") return // コメントかも
    if (anno["@type"]) return // type="tts" は重複なので不要
    const cp = anno["@cp"] as string // 絵文字
    const texts = anno["#text"] // パイプ区切りの日本語
    texts.split(/ *\| */).forEach((text, _, all) => {
      jas.add({ cp, text, all })
    })
  })
  return jas
}

function oldKanasInEmoJisyo(pfile: string): Map<string, string> {
  const oldRevs = new Map<string, string>()
  Deno.readTextFileSync(pfile)
    .split("\n")
    .forEach((line) => {
      if (line.length < 7 || line.startsWith(";")) {
        return
      }
      const [kana, _cp, _code, henkan] = delisp(line)
        .split(/ \/|;|, |\/$/, 4)
      if (kana == "undefined" || kana.includes("/")) {
        return
      }
      oldRevs.set(henkan.replace(" ", ""), kana) // ※
    })
  return oldRevs
}

function enlisp(s: string): string {
  return /[\/;]/.test(s)
    ? `(concat "${s.replace(/\//g, "\\057").replace(/;/g, "\\073")}")`
    : s
}

// 終わり判定が [;/] なので汎用ではない
function delisp(s: string): string {
  return s.replace(
    /\(concat "(.*?)"\)([;\/])/g,
    (_orig, ...args: string[]) =>
      args[0]
        .replace("\\057", "/")
        .replace("\\073", ";") +
      args[1],
  )
}

function hiragana(s: string | undefined): string {
  return String(s).replace(
    /[ァ-ヶ]/g,
    (kata) => String.fromCharCode(kata.charCodeAt(0) - 0x60),
  ).replace("ゔ", "う゛")
}
