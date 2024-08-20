/* ZIPCODE-MK の TypeScript 版
   なので GPL2 ライセンスです */

import * as csv from "jsr:@std/csv"
import * as fmt from "jsr:@std/datetime/format"
import * as cli from "jsr:@std/cli"
import { splitAddr2, splitAddr3 } from "./zipcode-split.ts"

function main(
  inCSV: string,
  outJisyo: string,
  outHeader: string,
  outGeo: string,
) {
  const geoMap = new Map<string, string>()
  const zipMap = new Map<string, string>()
  const text = Deno.readTextFileSync(inCSV)
  csv.parse(text)
    .forEach((entry: string[]) => {
      const it: Entry = {
        zip: entry[2],
        kana1: entry[3],
        kana2: entry[4],
        kana3: entry[5],
        addr1: entry[6],
        addr2: entry[7],
        addr3: entry[8],
      }
      addGeo(geoMap, it)
      let matches
      let _addr3
      let _addr4
      let stat
      if (
        RegExp([
          "^以下に掲載がない場合$",
          "一円$",
          "の次に番地がくる場合$",
          "^[０-９].*[０-９]$",
        ].join("|")).test(it.addr3)
      ) {
        it.addr3 = ""
      } else if (it.addr3 == "富岡（○○屋敷）") {
        it.addr3 = "富岡（大屋敷、中屋敷、東屋敷、西屋敷）"
      }
      if (it.addr3.includes("（")) {
        // 「ミッドランドスクエア（高層棟）（地階・階層不明）」に対応するため last
        const start = it.addr3.lastIndexOf("（")
        if (
          (matches = it.addr3.match(
            /^(.+)（([０-９]+階)）$/,
          ))
        ) { // 「サンシャイン６０　１階」など
          const optionalSpace = /[０-９]$/.test(matches[1]) ? "　" : ""
          it.addr3 = matches[1] + optionalSpace + matches[2]
        } else if (
          it.addr1 == "京都府" &&
          it.addr2.startsWith("京都市")
        ) {
          _addr4 = it.addr3.substring(0, start)
          _addr3 = it.addr3.substring(start)
          if (it.addr3.endsWith("）")) {
            it.addr3 = mkdicProcessKyoto(_addr3, _addr4, it.addr1 + it.addr2)
            _addr4 = undefined
          } else {
            it.addr3 = ""
            stat = true
          }
        } else if (
          it.addr3.includes("）") && !RegExp([
            "地割",
            "を除く",
            "を含む",
            "全域",
            "[ア-ン]、[ア-ン]",
          ].join("|")).test(it.addr3)
        ) {
          _addr4 = it.addr3.substring(0, start)
          _addr3 = it.addr3.substring(start)
          if (_addr4 == "甲、乙") {
            _addr4 = ""
          }
          if (
            _addr3.endsWith("）") && !RegExp([
              "「",
              "」",
              "〜",
              "[０-９]",
            ].join("|")).test(_addr3)
          ) {
            it.addr3 = mkdicProcessKakkonai(
              _addr3,
              _addr4,
              it.addr1 + it.addr2,
            )
            stat = false
            _addr4 = undefined
          } else {
            _addr4 = undefined
            _addr3 = undefined
            it.addr3 = it.addr3.substring(0, start)
            stat = true
          }
        } else {
          it.addr3 = it.addr3.substring(0, start)
          _addr3 = undefined
          stat = true
        }
      }
      if (it.addr3.endsWith("地割")) {
        if (
          (matches = it.addr3.match(
            /^([^０-９]+)([０-９]+)地割(、|〜)([^０-９]+)([０-９]+)地割$/,
          ))
        ) {
          const chimei = matches[1]
          const from = hankakuNumber(matches[2])
          const to = hankakuNumber(matches[5])
          it.addr3 = [...Array(1 + to - from)]
            .map((_, i) => i + from)
            .map((i, notFirst) =>
              (notFirst ? it.addr1 + it.addr2 : "") +
              chimei + zenkaku(i) + "地割"
            ).join("/")
        }
      }
      if (it.addr3.endsWith("）")) {
        if (_addr4 != undefined && _addr3 != undefined) {
          _addr3 += it.addr3
          it.addr3 = mkdicProcessKyoto(_addr3, _addr4, it.addr1 + it.addr2)
          stat = false
          _addr4 = undefined
        } else if (_addr3 != undefined) {
          it.addr3 = _addr3
          stat = false
        }
      }
      if (
        ["岩田町居村、北郷中", "岩田町宮下、道合", "飯村町西山、高山"]
          .includes(it.addr3)
      ) { // いずれも愛知県豊橋市
        if ((matches = it.addr3.match(/^(.+)町(.+)、(.+)$/))) {
          it.addr3 = matches[1] + "町" + matches[2] +
            "/" + it.addr1 + it.addr2 +
            matches[1] + "町" + matches[3]
        }
      } else if (it.addr3 == "井道、奥井道、内井道") {
        it.addr3 = it.addr3.split("、").map((v, notFirst) =>
          (notFirst ? it.addr1 + it.addr2 : "") + v
        ).join("/")
      }
      if (stat && _addr3 != undefined && _addr4 != undefined && it.addr3) {
        _addr3 += it.addr3
        it.addr3 = ""
      } else if (it.addr3.includes("、")) {
        if (stat) {
          if (_addr3 != undefined) {
            it.addr3 = _addr3
          } else {
            it.addr3 = ""
          }
        }
      }
      if (stat && it.addr3) {
        _addr3 = it.addr3
      } else if (!_addr4) {
        _addr3 = ""
      }
      const oldValue = zipMap.get(it.zip)
      zipMap.set(
        it.zip,
        (oldValue ? oldValue : "/") +
          `${it.addr1}${it.addr2}${it.addr3}/`,
      )
    })
  Promise.all([
    Deno.writeTextFile(outHeader, mkdicZipcodeHeader()),
    Deno.writeTextFile(
      outJisyo,
      okuriHeader() +
        [...zipMap.entries()].map(([k, v]) => `${k} ${v}\n`).join(""),
    ),
    Deno.writeTextFile(
      outGeo,
      okuriHeader() +
        [...geoMap.entries()].map(([k, v]) => `${k} ${v}\n`).join(""),
    ),
  ])
}

function mkdicProcessKyoto(n: string, c: string, prefix: string): string {
  let nantaras
  let matches
  if (RegExp(["〜", "（丁目）", "その他", "番地）$"].join("|")).test(n)) {
    nantaras = [""]
  } else if ((matches = n.match(/（([０-９]丁目)）/))) {
    c += matches[1]
    nantaras = [""]
  } else {
    nantaras = n.substring(1, n.length - 1).split("、")
  }
  return nantaras.map((nantara, notFirst) =>
    (notFirst ? prefix : "") + nantara + c
  ).join("/")
}

function mkdicProcessKakkonai(
  detail: string,
  cho: string,
  prefix: string,
): string {
  let matches
  const details = [""]
  if (RegExp(["〜", "（丁目）", "番地）$"].join("|")).test(detail)) {
    // すでに "" があるので何もしない
  } else if ((matches = detail.match(/^（([０-９]丁目)）$/))) {
    cho += matches[1]
  } else if (detail == "（地階・階層不明）") {
    details.push("地階")
  } else {
    details.push(...detail.substring(1, detail.length - 1).split("、"))
  }
  return details
    .filter((nantara) => !nantara.includes("その他"))
    .map((nantara, notFirst) => (notFirst ? prefix : "") + cho + nantara)
    .join("/")
}

function mkdicZipcodeHeader(): string {
  return `;; -*- coding: utf-8 -*-
;; SKK-JISYO.zipcode --- 7-digit ZIP code dictionary for SKK
;;
;; Copyright: Public domain dictionary.  Share and enjoy.
;;
;; Created: 24 Jul 2000
;; Time-stamp: <${fmt.format(new Date(), "yyyy-MM-dd")}>
;;
`
}

function okuriHeader(): string {
  return `;; okuri-ari entries.
;; okuri-nasi entries.
`
}

function hankakuNumber(z: string): number {
  return Number(
    z.replace(
      /[０-９]/g,
      (c) => String.fromCharCode(c.charCodeAt(0) - 0xFEE0),
    ),
  )
}

function zenkaku(h: number): string {
  return h.toString().replace(
    /[0-9]/g,
    (c) => String.fromCharCode(c.charCodeAt(0) + 0xFEE0),
  )
}

function hiragana(katakana: string): string {
  return katakana.replace(
    /[ァ-ヶ]/g,
    (c) => String.fromCharCode(c.charCodeAt(0) - 0x60),
  )
}

function addGeo(map: Map<string, string>, it: Entry) {
  map.set(hiragana(it.kana1), `/${it.addr1}/`)
  map.set(hiragana(it.kana2), `/${it.addr2}/`)
  splitAddr2(map, hiragana(it.kana2), it.addr2)
  splitAddr3(map, hiragana(it.kana3), it.addr3)
}

type Entry = {
  zip: string
  kana1: string
  kana2: string
  kana3: string
  addr1: string
  addr2: string
  addr3: string
}

const params = cli.parseArgs(Deno.args, {
  string: ["csv", "out", "header", "geo"],
  default: {
    csv: ".work/utf_ken_all.csv",
    out: ".work/.zipcode",
    header: "SKK-JISYO.zipcode",
    geo: ".work/.geo",
  },
})
main(params.csv, params.out, params.header, params.geo)
