# SKK dictionary files gh-pages

## 0. 各辞書の概略

SKK では個人の環境に応じて、また個人のニーズに応じて様々な辞書が利用できます。
例えば...

* 特にディスク容量にお困りでなければ、とりあえず `SKK-JISYO.L` をお使いになれば良
  いでしょう。
* Windows CE device や各種 PDA など、小さいディスク容量しか利用できない環境であれ
  ば、辞書を置けるディスク容量に応じ、`SKK-JISYO.S`, `SKK-JISYO.M` または `SKK-JISYO.ML` を
  お使いになれば良いでしょう。
* SKK abbrev 変換で、英語 → 日本語な変換を多用したい人は、英和辞書 `SKK-JISYO.edict` を
  `SKK-JISYO.L` などと併用すれば良いでしょう。
* 地名を強化したい場合は、地名辞書 `SKK-JISYO.geo` を併用すれば良いでしょう。

各辞書は、[skktools](https://github.com/skk-dev/skktools) を使って加除ができます。
併用したい辞書をマージし、あるいは辞書の引き算をすることでスリム化を図りましょう。

複数の辞書を検索指定できる skk 辞書サーバもあります。

SKK 辞書の誤り、追加すべき候補、エントリを発見された方は多少にかかわらず <skk@ring.gr.jp> までお知らせください。


## 1. 基本辞書

SKK 入力のための基本となる、単独で日本語文の入力が可能になるよう設計された辞書です。
サイズが小さなものから順に、`SKK-JISYO.S`, `SKK-JISYO.M`, `SKK-JISYO.ML`, `SKK-JISYO.L` があります。好みや必要に応じて、どれか一つをダウンロードしてお使いください。

単漢字主義のS辞書と語彙主義のL辞書とでは収録されている「語」の種類だけでなく登録順なども大きく異なっているので、どの辞書を使うかでほとんど別の input method と言って良いほど使用感が違うと思います。

  * SKK-JISYO.S.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.S.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.S.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.S.gz.md5)]
    - 最も小さな辞書であり、`~/.skk-jisy`o に rename して最初の個人辞書のベースと
      して利用することができます。
    - 漢字へのアクセスを優先するために、使用頻度が少なくても収録されている候補があ
      ります。

  * SKK-JISYO.M.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.M.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.M.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.M.gz.md5)]
    - ミドルサイズの辞書です。

  * SKK-JISYO.ML.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.ML.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.ML.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.ML.gz.md5)]
    - M 辞書と L 辞書の中間サイズの辞書です。

  * SKK-JISYO.L.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.L.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.L.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.L.gz.md5)]
    - 最も大きな辞書です。ある程度の人名・地名や複合語までを含んでいます。
      annotation を積極的に付けることが推奨

  * SKK-JISYO.L.unannotated.gz
5    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.L.unannotated.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.L.unannotated.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.L.unannotated.gz.md5)]
    - L 辞書から annotation を取り除いたもの。make 時に自動的に生成されます。


## 2. 専門辞書

専門辞書は、基本辞書と併用して、必要に応じて各分野の語彙を増強するための辞書です。

`SKK-JISYO.L` をお使いで、PC に余力がおありなら、各種の固有名詞を体系的に補う

  - `SKK-JISYO.jinmei`
  - `SKK-JISYO.geo`
  - `SKK-JISYO.station`
  - `SKK-JISYO.propernoun`

の併用をお勧めします。

### (1) 固有名詞系

  * SKK-JISYO.jinmei.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.jinmei.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.jinmei.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.jinmei.gz.md5)]
    - 人名、特に日本人の姓名を収録した辞書です。
    - GPL に基づき改変、再配布可能な Wnn 用の人名辞書 (gerodic) を SKK 形式に変換
      したものを出発点に、海道昭恵氏作の [人名録](http://www.ctk.ne.jp/~kai-6344/)
      のデータを追加し、さらに DDSKK 独自の追加・編纂も施して作成されています。

  * SKK-JISYO.fullname.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.fullname.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.fullname.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.fullname.gz.md5)]
    - 海道昭恵氏の公開されておられる音訳用の [人名録](http://www.ctk.ne.jp/~kai-6344/)
      を SKK 辞書形式に変換したものをベースに加除したものです。新聞に掲載された人
      物のフルネームを、「当時の」肩書き付きで収めた辞書です。
    - なお、この辞書の人名を姓と名に分離したものは `SKK-JISYO.jinmei` にも収録させ
      て頂いております。

  * SKK-JISYO.geo.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.geo.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.geo.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.geo.gz.md5)]
    - 日本郵便株式会社（以前は郵政省 → 郵政事業庁 → 日本郵政公社）のページにある
      データをもとに SKK 辞書形式の「地名辞典」に加工したものです。
    - 2015 年 2 月 3 日現在、 http://www.post.japanpost.jp/zipcode/dl/readme.html に
      よれば、『郵便番号データに限っては日本郵便株式会社は著作権を主張しません。自
      由に配布していただいて結構です。』とあります。同社のご厚意に感謝しつつ、自由
      に配布できるよう、明示的に GPL を適用しました。
    - この辞書については、DDSKK 独自の加除や加工がかなり加えられており、単純なコン
      バートとは性質をやや異にしています。
    - 元データの性質上、住所という形を取らない地名や地域名については残念ながら現段
      階ではあまり強くはありません。

  * SKK-JISYO.propernoun.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.propernoun.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.propernoun.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.propernoun.gz.md5)]
    - 芸能・音楽・企業・アニメなど、`SKK-JISYO.geo` にも `SKK-JISYO.jinmei` 辞書に
      も当てはまらない固有名詞を集めたものです。
    - 収録されている語彙の詳細については、辞書ファイルのヘッダを参照してください。
      一例として、東証一部上場企業や国内の四年制大学などを網羅しています。

  * SKK-JISYO.station.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.station.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.station.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.station.gz.md5)]
    - 駅名・路線名・鉄道会社名およびその他の鉄道用語の専門辞書です。
      国内の鉄道駅名はほぼ網羅できているのではないかと思います。


### (2) 専門分野系

  * SKK-JISYO.law.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.law.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.law.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.law.gz.md5)]
    - 弁護士 小松 弘先生による GPL な法律辞書 [法律用語電子化辞書 LKKS](http://icrouton.as.wakwak.ne.jp/pub/kks/index.html) を SKK 辞書形式に変換したものです。
    - `SKK-JISYO.L` と重複するコンテンツを機械的に削除し、その後、更に一般的用語は
      手作業で削除して `SKK-JISYO.L` に移しています。一部追加もあります。

  * SKK-JISYO.okinawa.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.okinawa.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.okinawa.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.okinawa.gz.md5)]
    - [沖縄辞書プロジェクト](http://www.zukeran.org/o-dic/) が作成、メンテナンスし
      ている沖縄辞書を SKK 辞書形式に変換したものです。沖縄辞書に付いていた品詞や
      コメントは、annotation に変換して残しています。

  * SKK-JISYO.china_taiwan.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.china_taiwan.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.china_taiwan.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.china_taiwan.gz.md5)]
    - 中国、台湾の地名ばかりを集めた辞書です。簡単な annotation が付いています。


### (3) 特殊変換系

  * SKK-JISYO.assoc.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.assoc.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.assoc.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.assoc.gz.md5)]
    - この辞書は、見出し語を単純に変換して得られる候補ではなく、見出し語と候補との
      間に一定の連想関係があるエントリを集めたものです。大量の略語エントリを含んで
      いるので、略語を用いて積極的に入力の省力化を図りたいという方にお勧めします。
    - この辞書については事前に収録内容を知らないと使いづらい部分がありますので、辞
      書ファイルのヘッダにある解説に目を通しておかれると良いでしょう。

  * SKK-JISYO.edict.tar.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.edict.tar.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.edict.tar.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.edict.tar.gz.md5)]
    - 英数文字を見出しにした変換 (abbrev) によって英和変換を実現するための辞書です。
    - [The Electronic Dictionary Research and Development Group](http://www.csse.monash.edu.au/groups/edrdg/) による「和英辞典」edict を `edict2skk.awk` を利用して SKK 辞書形式の「英和辞典」に加工したものです。
    - edict も独自の copyright notice を設けており、`SKK-JISYO.edict` もこの配布条
      件に拘束されます。配布条件を簡単に言うと
          無償で配布する場合、ドキュメント `edict_doc.txt` を常に一緒に配布し、
          the copyright notice を取り外さない限り、配布可能
      というものです。
      詳細は http://www.csse.monash.edu.au/groups/edrdg/newlic.html, `SKK-JISYO.edict` の
      ヘッダー及び `edict_doc.txt` などをご覧下さい。

  * zipcode.tar.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/zipcode.tar.gz](https://github.com/skk-dev/dict/raw/gh-pages/zipcode.tar.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/zipcode.tar.gz.md5)]
    - 郵便番号辞書です。日本郵便株式会社が公開しているデータから変換したものです。


## 3. 特殊辞書

特殊な入力方法や環境整備を必要とする辞書です。

  * SKK-JISYO.JIS2.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS2.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS2.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS2.gz.md5)]
    - この辞書は JIS 第二水準の文字のみを、部首を見出しとして収録した SKK 辞書です。
      `skk-tankan.el` と併用すると、部首と画数から検索できるようになります。

  * SKK-JISYO.JIS3_4.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS3_4.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS3_4.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS3_4.gz.md5)]
    - この辞書は、JIS 第三、第四水準の文字に代表される、JISX0208 には含まれないが
      JISX0213 には含まれる文字およびそれらを含む語彙のみを収録した SKK 辞書です。
    - この辞書は、下記の複数のコンテンツを含んでおりますが、辞書構成については独自
      に行われたものです。
    - `SKK-JISYO.JIS3_4.gz` は
      + KAWABATA, Taichi さんによる 「JIS X 0213 漢字辞書 (最終更新:2000/3/5)」(`tankan3.u` 及び `kigou.u`) (http://sr3.xoom.com/golconda_99/jisx0213/index.html にあったんですが、リンク切れのようです。)

      + もりみつじゅんじさんによる jisx0213code.txt

      をベースに SKK 辞書形式に変換し、加筆、修正したものです。

| ファイル名          | 解説                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|---------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| JIS X 0213 漢字辞書 | `tankan3.u` については OMRON SOFTWARE Co., Ltd. により元来 GPL に基づき配布されていたものであり、`kigou.u` は川幡さんのオリジナルです。川幡さんには `kigou.u` の GPL に基づく配布をご快諾いただきました。<br>なお、`hojo.u` については、イニューシステムが販売している補助漢字フォントの付属をベースにして製作されているとのことで、イニューシステムに問合せをしましたが、回答が得られませんでしたのでこれを含めていません。従い `hojo.u` を直接変換する形でこのファイルに候補を取り込むことは見合わせたいと思います。イニューシステムへの問合せについては、`hojo.u` の作者である Hirofumi Fujiwara さんにお世話になりました。 |
| jisx0213code.txt    | もりみつじゅんじさんに GPL に基づく配布をご快諾いただきました。Yano Keisukeさんに情報提供をいただく他、辞書形式の変換や配布条件の確認などのお手伝いをいただきました。関係された皆様に厚く感謝申し上げます。                                                                                                                                                                                                                                                                                                                                                                                                                    |

  * SKK-JISYO.JIS2004.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS2004.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS2004.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.JIS2004.gz.md5)]
    - JIS X 0213 の 2004 年改正で追加された「表外漢字 UCS 互換」10 文字を含む語彙
      のみを収録した辞書です。
    - 本来は `SKK-JISYO.JIS3_4` の一部となるべきものですが、JIS X 0213:2004 に対応
      していない環境を考慮して、当面別ファイルとするものです。`SKK-JISYO.JIS3_4` と
      あわせてお使いください。

  * SKK-JISYO.itaiji.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.itaiji.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.itaiji.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.itaiji.gz.md5)]
    - 「▽」の挿入による再変換の機構などを利用して、異体字変換を行うための辞書です。
      斉藤寿成氏、大野裕氏の作成された [異體字轉](http://www.eonet.ne.jp/~kotobukispace/ddt/itaizy/itaizy.html) から作成されています。

  * SKK-JISYO.itaiji.JIS3_4.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.itaiji.JIS3_4.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.itaiji.JIS3_4.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.itaiji.JIS3_4.gz.md5)]
    - `SKK-JISYO.itaiji` の JIS 第３・第４水準版です。もりみつじゅんじ氏の作成によ
      る [参照文字リスト(0213)](http://www.jca.apc.org/~earthian/aozora/0213.html) か
      ら作成されています。

  * SKK-JISYO.mazegaki.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.mazegaki.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.mazegaki.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.mazegaki.gz.md5)]
    - `SKK-JISYO.M` を `skk-mkmgk.el` で機械的に加工した交ぜ書き辞書です。
      `skk-tutcode` を利用する場合に併用すると便利です。

  * SKK-JISYO.lisp.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.lisp.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.lisp.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.lisp.gz.md5)]
    - `SKK-JISYO.L` から concat 関数を除く Lisp の関数の候補を抜き出した辞書です。
    - 新規に `skk-relative-date` 関数を追加しています。プログラム実行変換で用いら
      れる Lisp の関数を拡張する際はこの辞書を更新して頂ければと思います。


## 4. 編纂用辞書

ユーザが直接変換に使うというよりは、主に他の辞書を編纂するために補助的に使われる辞
書です。

  * SKK-JISYO.wrong.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.wrong.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.wrong.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.wrong.gz.md5)]

  * SKK-JISYO.wrong.annotated.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.wrong.annotated.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.wrong.annotated.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.wrong.annotated.gz.md5)]
    - `SKK-JISYO.{SML}` に間違えて含まれていたエントリを抽出したものです。
    - このファイルは `SKK-JISYO.{SML}` に再び誤ったエントリが含まれないようにする
      ための指標であり、また、個人辞書から誤ったエントリを削除するためのファイルで
      もあります。
    - 個人辞書から誤ったエントリを取り除くには、`skktools` を利用し、次のようにすれば良いでしょう。

      ```
      % skkdic-expr ~/.skk-jisyo - SKK-JISYO.wrong | skkdic-sort - >.skk-jisyo.new
      ```

    - または、`skkdic-expr2` なら annotation に対応しているので、

      ```
      % skkdic-expr2 ~/.skk-jisyo - SKK-JISYO.wrong.annotated >.skk-jisyo.new
      ```

      のように、`SKK-JISYO.wrong.annotated` をそのまま使用することもできます。

  * SKK-JISYO.pubdic+.gz
    - [https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.pubdic+.gz](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.pubdic+.gz)
      [[md5](https://github.com/skk-dev/dict/raw/gh-pages/SKK-JISYO.pubdic+.gz.md5)]
    - `SKK-JISYO.pubdic+` は the Pubdic+ project により作成された pubdic.p を SKK
      辞書形式に加工し、L 辞書に含まれるエントリを削除したものです。
    - pubdic.p には the Pubdic+ project 独自の配布条件が付けられており `SKK-JISYO.pubdic+`
      はこれを継承します。配布条件を一言で言うと「いかなる利用方法も可」というもの
      です。詳細は `SKK-JISYO.pubdic+` のヘッダー部分をご覧下さい。
    - 現在ではほとんどの語が `SKK-JISYO.L` などに取り込まれるか削除されるかしたた
      め、この辞書をユーザが使う意味はそれほどないのではと思われます。
