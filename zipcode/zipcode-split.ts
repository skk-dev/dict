// skkdic-split-geo.rb みたいなやつ
// このあと skkdic-expr2 zipcode/.work/.geo - SKK-JISYO.L - SKK-JISYO.geo を確認

// もっと区切りたければ前のエントリと比較して共通部分を取り出したりもできるかも

export function splitAddr2(
  map: Map<string, string>,
  kana: string,
  addr: string,
) {
  let matches
  if ((matches = addr.match(/^(.+市)(.+区)$/))) {
    const addrs = [matches[1], matches[2]]
    // 広島市と北九州市に「し」が含まれる
    if ((matches = kana.match(/^(.+?(?<!ひろ|きたきゅう)し)(.+く)$/))) {
      const kanas = [matches[1], matches[2]]
      setIfSameLength(map, kanas, addrs)
    } else {
      console.error("カナと住所の分割不一致", addrs)
      console.error(kana, addr)
    }
  } else if ((matches = addr.match(/^(.+郡)(.+[町村])$/))) {
    const addrs = [matches[1], matches[2]]
    // 北群馬郡に「ぐん」が含まれる
    if (
      (matches = kana.match(
        /^(.+?ぐん(?!まぐん))(.+(?:まち|ちょう|むら|そん))$/,
      ))
    ) {
      const kanas = [matches[1], matches[2]]
      setIfSameLength(map, kanas, addrs)
    } else {
      console.error("カナと住所の分割不一致", addrs)
      console.error(kana, addr)
    }
  }
}

export function splitAddr3(
  map: Map<string, string>,
  kanaKu: string,
  addrKu: string,
) {
  let kana = kanaKu
  let addr = addrKu

  // 区を先に取り除く
  let matches
  if (
    (matches = addr.match(
      /^([^−（０-９、）「」〜一二三四五六七八九十]+区(?!画)|三和区)(.+)$/,
    ))
  ) {
    const addrs = [matches[1], matches[2]]
    if ((matches = kana.match(/^(.+?(?<!いた)く)(.+)$/))) {
      const kanas = [matches[1], matches[2]]
      map.set(kanas[0], `/${addrs[0]}/`)
      kana = kanas[1]
      addr = addrs[1]
    } else {
      console.error("カナと住所の分割不一致", addrs)
      console.error(kana, addr)
    }
  } // 読みのある京都地名は取っておく
  else if (
    (matches = kana.match(
      /^(.+)（(.+どおり.+(?:あがる|さがる|にしいる|ひがしいる).*)）$/,
    ))
  ) {
    kana = matches[1]
    const kanaKyoto = matches[2]
    if ((matches = addr.match(/^(.+)（(.+)）$/))) {
      addr = matches[1]
      const addrKyoto = matches[2]
      const kanaKyotos = kanaKyoto.split("、")
      const addrKyotos = addrKyoto.split("、")
      if (kanaKyotos.length == addrKyotos.length) {
        kanaKyotos.forEach((_, i) =>
          splitKyoto(map, kanaKyotos[i], addrKyotos[i])
        )
      } else {
        console.error("京都の地名データ不整合")
        console.error(kanaKyotos, addrKyotos)
      }
    } else {
      console.error("京都の地名データ不整合")
      console.error(kana, kanaKyoto, addr)
    }
  }

  // 条丁目の正規表現
  const kanaJouChoume = "(?:" +
    "(?<!ＳＳ３?)[−（０-９、）＜＞]+" + "(?:" + // ＳＳ３０というビル
    "(?:" + [
    "(?<!（)じょう", // 「十三（城）」というケースに対応
    "(?:いっ)?ちょうめ",
    "のちょう",
  ].join("|") + ")?" +
    ")" +
    ")+"
  const addrJouChoume = "(?:(?<!ＳＳ３?)[−（０-９、）「」〜]+(?:条|丁目)?)+"
  // 区切りきれなかった場合には漢数字も使ってリトライ
  const addrJouChoume2 = "(?:" +
    "(?:" + [
    "[−（０-９、）「」〜]+",
    "[一二三四五六七八九十]+(?!十四軒|四軒|重麦|十嵐|[嵐角百軒])",
  ].join("|") + ")" +
    "(?:条|丁目|の町)?" +
    ")+"

  // 町を「ちょう」と読む正規表現
  const KanaChouExcludedPrefix = "(?<!" + [
    "^.", // 銀杏
    "^.ん", // 番帳免, 新丁, 三町免, 本町通 (ほんちょう は例外処理)
    "^ほう", // 包丁
    "^たてやま", // 立山頂上 (館山町○○という地名がないのでギリOK)
    "^(?:だ|ら)い", // 雷鳥沢, 大超寺奥
    "^(?:たまがわ)?でんえん", // 田園調布
    "^みかつきちょう", // 三日月町長神田
    "っ",
    "きたまん", // .北万丁目
  ].join("|") + ")"
  const KanaChouExcludedSuffix = "(?!" + [
    ".?ちょう$",
    "えん$", // 翠鳥園
    "けいだいら$", // 長慶平
    "(?:こ)?だい$", // 女満別眺湖台, 白鳥台
    "し$", // 調子
    "すけまき$", // 大森町長助巻
    "だ$",
    "ちんちょう$", // 髭茶屋桃燈町
    "ばる$", // 城島町六町原
    "ぶ$", // 「町歩」という単位みたいなもの？
    "ほう$", // 庄内町(東|西)長宝
    "まち$",
    "め(?:かみ|しも|ほんどおり)?$",
    "(?:め|きち)(?:きた|ひがし|みなみ|なか|にし)?(?:まち)?$",
    "ろう$",
    "(?:ちょう)?じゃ", // 長者
    "のめ", // 宮城県 仙台市若林区 六丁の目.町
    "(?:きゅう|ぞう|ふく|みょう)じ", // 寺
    ".んじ$", // この $ を外すと 栗沢町万字○○ や 桃山町弾正島 に影響
    "えんじまち", // 市谷長延寺町
  ].join("|") + ")"
  const kanaChou = KanaChouExcludedPrefix + "ちょう" + KanaChouExcludedSuffix

  // 町を「まち」と読む正規表現
  const kanaMachiExcludedPrefix = "(?<!" + [
    "^あさづ", // 朝妻筑摩, 朝妻町
    "かし", // 鹿島千年町, 鹿島町, 上中島町, 北高島町など
  ].join("|") + ")"
  const kanaMachiExcludedSuffix = "(?!" + [
    "ん",
    "ゃ",
    "ゅ",
    "ょ",
    "くぜん", // 桃山筑前台町
    "い(?:した)?$",
    "ごや$", // 前沢鳥待小屋
    "さきうみほたる$", // 中島地先海ほたる
    "だ$", // 原町田, 本町田
    "はらだに$", // 美濃山千原谷
    "ば$",
    "よがみず$", // 山川浜児ケ水
    "りっぷ$", // 丸山散布
  ].join("|") + ")"
  const kanaMachi = kanaMachiExcludedPrefix + "まち" + kanaMachiExcludedSuffix

  // その他の切り方 (完璧を目指すと増えるが、それなら手作業の方が良さそうかも)
  const kanaOthers = [
    "[Ａ-Ｚ・]+(?:とう)?[（、]",
    "(?<=^(?:" + [ // 「次のビルを除く」で検索して適当に
      "あかさか(?=あかさか)",
      "あたご(?=あたご)",
      "あべのすじ",
      "えびす(?=えびす)",
      "おおよどなか",
      "かすみがせき(?=かすみがせき)",
      "こうなん(?=しながわ)",
      "しぶや(?=しぶや)",
      "しんとしん",
      "ちゅうおう(?=あえる|ＳＳ)",
      "とよす(?=とよす)",
      "とらのもん",
      "ながたちょう",
      "なかのしま(?=なかのしま)",
      "にししんじゅく",
      "にほんばし",
      "ひがししんばし",
      "まるのうち(?!(?:こうえん)?まち$|(?:ようせい)?ちょう$)",
      "みた(?=すみとも)",
      "ろっぽんぎ",
    ].join("|") + "))",
    "いちえん$",
    "ごう(?=たのつだんち)", // ウチハシニシ（３チョウメ１１バン５−８ゴウタノツダンチ）
    // 本来はデータのほうで ゴウ＜タノツダンチ＞ と区切るのが正しいはず
    "ちさき",
    "(?<=^ほんちょう)(?=.+ちょうめ)", // ^.ん の救済
    "をのぞく",
    "をふくむ）$",
  ].join("|")
  const addrOthers = [
    "[Ａ-Ｚ・]+(?:棟)?[（、]",
    "(?<=^(?:" + [
      "赤坂(?=赤坂)",
      "愛宕(?=愛宕)",
      "阿倍野筋",
      "恵比寿(?=恵比寿)",
      "大淀中",
      "霞が関(?=霞が関)",
      "港南(?=品川)",
      "渋谷(?=渋谷)",
      "新都心",
      "中央(?=アエル|ＳＳ)",
      "豊洲(?=豊洲)",
      "虎ノ門",
      "永田町",
      "中之島(?=中之島)",
      "西新宿",
      "日本橋",
      "東新橋",
      "丸の内(?!(?:公園)?町$)",
      "三田(?=住友)",
      "六本木",
    ].join("|") + "))",
    "一円$",
    "地先",
    "(?<=^湊通丁)", // 普段この粒度は無視するが折角「ちょう」で切れているので
    "を除く",
    "を含む）$",
  ].join("|")

  const kanaSplitter = kanaJouChoume + "|" +
    "(?<=" +
    "^[^−（０-９、）＜＞]+" + "(?:" + kanaChou + "|" + kanaMachi + "))|" +
    kanaOthers
  const kanas = kana.split(RegExp(kanaSplitter))

  // 「町」で区切る正規表現 (一文字の地名は条件を緩めて 2 でリトライ)
  const addrExcludedPrefix = "(?<!六|壱)" // 城島町+六町原, 三潴町壱町原
  const addrExcludedSuffix = "(?!井$|目$|歩$|田$|野$)" // 田 と 野 は合致し過ぎる
  const addrExcludedSuffix2 = "(?!井$|目$|歩$)" // (新|上)町+野, 瀬峰町+田
  const addrSplitter = addrJouChoume + "|" +
    "(?<=" +
    "^[^−（０-９、）「」〜]+" + addrExcludedPrefix + "町" + addrExcludedSuffix +
    ")|" +
    addrOthers
  const addrs = addr.split(RegExp(addrSplitter))

  if (setIfSameLength(map, kanas, addrs)) {
    return
  } else {
    if (kanas.length > addrs.length) {
      const addrSplitter2 = addrJouChoume2 + "|" +
        "(?<=" +
        "^[^−（０-９、）「」〜]+" +
        addrExcludedPrefix + "町" + addrExcludedSuffix2 +
        ")|" +
        addrOthers
      const addrs2 = addr.split(RegExp(addrSplitter2))
      if (!setIfSameLength(map, kanas, addrs2)) {
        console.error("カナと住所の分割不一致", kanas, addrs)
        console.error("カナと住所の分割不一致", kanas, addrs2)
        console.error(kana, addr)
      }
    } else {
      if (
        kanas.length == 0 ||
        kanas.length == 1 &&
          addrs.some((v) =>
            v.endsWith("上る") || v.endsWith("下る") ||
            v.endsWith("西入") || v.endsWith("東入")
          ) // 面白い地名なのに読みデータがないので使えない
      ) {
        return
      }
      const addrs2 = addr.split(/[−（０-９、）〜]+/)
      if (!setIfSameLength(map, kanas, addrs2)) {
        console.error("カナと住所の分割不一致", kanas, addrs)
        console.error("カナと住所の分割不一致", kanas, addrs2)
        console.error(kana, addr)
      }
    }
  }
}

function splitKyoto(map: Map<string, string>, kana: string, addr: string) {
  let matches
  if (
    (matches = kana.match(
      /^(.+(?:どおり|すじ))(.+?)(?:あがる|さがる|にしいる|ひがしいる)/,
    ))
  ) {
    const kanaDooris = [matches[1], matches[2] + "どおり"]
    if ((matches = addr.match(/^(.+(?:通|筋))(.+?)(?:上る|下る|西入|東入)/))) {
      const addrDooris = [matches[1], matches[2] + "通"]
      setIfSameLength(map, kanaDooris, addrDooris)
    } else {
      console.error("京都の地名データ不整合")
      console.error(kanaDooris, addr)
    }
  } else {
    console.error("京都の地名データ不整合")
    console.error(kana, addr)
  }
}

function setIfSameLength(
  map: Map<string, string>,
  kanas: string[],
  addrs: string[],
): boolean {
  const excludedAddrs = [
    "^[ア-ン][甲乙丙丁]?$",
    "○○屋敷",
    "の次に$",
    "くる場合$",
    "^以下に掲載がない場合$",
    "^その他$",
    "^地階・階層不明$",
  ].join("|")
  if (kanas.length != addrs.length) {
    return false
  }
  kanas.forEach((v, i) => {
    const kana = v
      .replaceAll(/[．・]/g, "")
      .replaceAll("ゔ", "う゛") // ??
      .replace(/^[１-９]じょうどおり$/, "") // 京都は条丁目処理を通らない
      .replace("ＳＳ３０", "えすえすさーてぃー") // ビル
      .replace("ＯＡＰ", "おーえーぴー") // ビル
    const addr = addrs[i]
    if (
      kana.length && addr.length &&
      !RegExp(excludedAddrs).test(addr)
    ) {
      map.set(kana, `/${addr}/`)
    }
  })
  return true
}
