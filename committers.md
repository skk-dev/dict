SKK 辞書 committer のみなさまへ
===============================

# 1. ライセンス
## 1.1. GPL が適用される辞書

- SKK-JISYO.{SML}
- SKK-JISYO.JIS2
- SKK-JISYO.JIS3_4
- SKK-JISYO.JIS2004
- SKK-JISYO.geo[^1]
- SKK-JISYO.law
- SKK-JISYO.wrong
- SKK-JISYO.jinmei
- SKK-JISYO.lisp
- SKK-JISYO.assoc
- SKK-JISYO.china_taiwan
- SKK-JISYO.fullname
- SKK-JISYO.hukugougo
- SKK-JISYO.mazegaki
- SKK-JISYO.pinyin
- SKK-JISYO.propernoun
- SKK-JISYO.station
- SKK-JISYO.itaiji.JIS3_4[^2]
- SKK-JISYO.noregist
- SKK-JISYO.not_wrong
- SKK-JISYO.requested
- SKK-JISYO.wrong.annotated[^3]

上記の各辞書については、 the Free Software Foundation が発行している
the GNU General Public License version 2 以降が適用されます。詳細につ
いては COPYING をご覧下さい。

[^1]: geo に関しては 2.3. も参照。

[^2]: itaiji は GPL ではない。

[^3]: wrong は wrong.annotated から自動生成。

## 1.2. GPL 以外のライセンスが適用される辞書

### 1.2.1. SKK-JISYO.pubdic+

SKK-JISYO.pubdic+ は、 the Pubdic+ project によって作成された pubdic.p
を SKK 辞書形式に加工したものです。
pubdic.p には the Pubdic+ project 独自の配布条件が付けられていますので、
SKK-JISYO.pubdic+ はこれを継承します。

配布条件を一言で言うと「いかなる利用方法も可」というものです。詳細は
SKK-JISYO.pubdic+ のヘッダー部分をご覧下さい。

### 1.2.2. SKK-JISYO.edict, SKK-JISYO.edict2

SKK-JISYO.edict は [The Electronic Dictionary Research and Development
Group](http://www.edrdg.org/) による「和英辞典」edict を tools/convert2skk/edict2skk.awk
を利用して SKK 辞書形式の「英和辞典」に加工したものです。

同じく、SKK-JISYO.edict2 は edict2u を加工したものです。

edict, edict2u ともに Creative Commons Attribution-ShareAlike Licence (V3.0)
ですので、それらの派生である SKK-JISYO.edict, SKK-JISYO.edict2 も同じ
ライセンスとしました。

### 1.2.3. SKK-JISYO.geo

SKK-JISYO.geo は、日本郵便のページにあるデータをもと
に SKK 辞書形式の「地名辞典」に加工したものです。

2024 年 8 月 17 日現在、https://www.post.japanpost.jp/zipcode/dl/readme.html
によれば、

    使用・再配布・移植・改良について
    郵便番号データに限っては日本郵便株式会社は著作権を主張しません。自由に配布していただいて結構です。

とあります。同社のご厚意に感謝しつつ、自由に配布できるよう、明示的に
GPL を適用しました。

### 1.2.4. SKK-JISYO.okinawa

この辞書は Public Domain です。使用・変更・配布に関しては一切の制限を
つけません。商品などに組み込むことも自由に行なってください。

### 1.2.5. SKK-JISYO.office.zipcode, SKK-JISYO.zipcode

geo と同じデータに基づきますが、機械的に抽出しているため
元のデータと同じく Public domain です。

ただし、これらを生成するためのプログラムは GPL が適用されます。
[zipcode/README.md](zipcode/README.md) をご覧下さい。

### 1.2.6. SKK-JISYO.emoji

SKK-JISYO.emoji は、[Unicode Common Locale Data Repository](http://cldr.unicode.org/)
の cldr-common.zip に含まれる annotations/(en|ja).xml から生成していま
す。

unicode license です。

### 1.2.7. SKK-JISYO.ivd

SKK-JISYO.ivd は、 https://unicode.org/ の ivd/data/*/IVD_Sequences.txt
から生成しています。

unicode license です。

### 1.2.8. SKK-JISYO.itaiji

Public domain です。


# 2. 更新手順

## 2.1. 基本

GitHub でブランチを作って
JSON と ChangeLog を更新し、
プルリクエストを出します。

ChangeLog を更新しなかった場合の ChangeLog、
および従来からの形式の辞書は、
GitHub Actions で自動更新されます。

### 2.1.1. JSON

json/SKK-JISYO.*.json 内の各エントリは以下の形式です。

```
    {
      "かな": [
        { "漢字1": [ "注釈1", "注釈2" ] },
        { "漢字2": [] }
      ]
    },
```

上記エントリは

```
かな /漢字1;注釈1;注釈2/漢字2/
```

に変換されます。

## 2.2. 自動生成など

### 2.2.1. 更新しないもの

- pubdic+
- edict
- requested (他の辞書に登録してここから減らすのはあり)
- wrong (annotated の方を編集する)

### 2.2.2. SKK-JISYO.china_taiwan

csv/china_taiwan.csv を更新して make します。
そこから script/txt2json.ts で JSON に戻します。

つまり、最低限プルリクエストに含めるのは CSV と JSON です。

### 2.2.3. SKK-JISYO.edict2

```
make SKK-JISYO.edict2
```

と実行することで生成できます。
JSON も更新されます。

edrdg.org で更新される以外に
こちらで更新することはありません。

### 2.2.4. SKK-JISYO.emoji

https://cldr.unicode.org/index/downloads を確認して、
Makefile 内の `CLDR_VER` と `CLDR_COMMON_VER` を更新します。

その後、漢字を平仮名にします。
まず既存の SKK-JISYO.emoji.predef を最新の ja.xml と
SKK-JISYO.L+ で更新します。

```
make SKK-JISYO.emoji.predef
```

更新された SKK-JISYO.emoji.predef を編集して、
複数候補のあるものや undefined をなくします。

```
undefined /🚙;U+1f699;RV車, (SUV車, アールブイ車, 乗り物, 自動車, 車)/
きんにく/すじにく /💪;U+1f4aa;筋肉, (ムキムキ, 力こぶ, 筋トレ)/
```

を

```
あーるぶいしゃ /🚙;U+1f699;RV車, (SUV車, アールブイ車, 乗り物, 自動車, 車)/
きんにく /💪;U+1f4aa;筋肉, (ムキムキ, 力こぶ, 筋トレ)/
```

にするような感じです。
ただし、「あーるぶいしゃ」は
右の括弧内にある「アールブイ車」でカバーされているので、
undefined のままにしておいても同じです。
そういう場合は「rvしゃ」などにしてカバー率を上げることもできます。

SKK-JISYO.emoji.predef の更新が終わったら、それを使って
SKK-JISYO.emoji を更新します。

```
make SKK-JISYO.emoji
```

そこから script/txt2json.ts で JSON に戻します。

```
rm json/SKK-JISYO.emoji.json   # Makefile の関係で一度消してください
make json/SKK-JISYO.emoji.json
```

最低限プルリクエストに含めるのは
SKK-JISYO.emoji.predef と JSON です。

ライセンスも変更されることがあるので、
unicode-license.txt も確認します。
変更されていたら yaml も更新してください。


### 2.2.5. SKK-JISYO.geo

手作業と自動生成情報のハイブリッドです。

#### 2.2.5.1 手作業で JSON から更新する場合

基本どおりです。
情報源について meta/SKK-JISYO.geo.yaml を更新することも忘れずに。

#### 2.2.5.2. zipcode から更新する場合

必要なら zipcode から抽出して
SKK-JISYO.geo にマージしてから script/txt2json.ts で JSON に戻します。

```
cd zipcode
autoconf -f                  # configure を生成
sh configure                 # Makefile を生成
make batch geo               # 最新データで更新
cd ..
emacs SKK-JISYO.geo.add      # 追加エントリを取捨選択
skkdic-expr2 SKK-JISYO.geo + SKK-JISYO.geo.add > SKK-JISYO.geo.new
mv SKK-JISYO.geo.new SKK-JISYO.geo
rm json/SKK-JISYO.geo.json   # Makefile の関係で一旦消す
make json/SKK-JISYO.geo.json # そのあとで JSON にする
```

meta/SKK-JISYO.geo.yaml の最終更新日も更新します。

プルリクエストに含めるのは YAML と JSON です。

(この場合はおそらく zipcode も更新が必要)

### 2.2.6. SKK-JISYO.hukugougo

ヘッダには「機械的に抽出」とありますが不明です。
基本どおり JSON を編集します。

### 2.2.7. SKK-JISYO.itaiji

ヘッダに書かれているデータは 404 エラーです。
基本どおり JSON を編集します。

### 2.2.8. SKK-JISYO.itaiji.JIS3_4

ヘッダに書かれた方式で直接編集します。
(JSON は存在しない: UTF-8 で itaiji に統合する予定だから？)

### 2.2.9. SKK-JISYO.ivd

https://www.unicode.org/ivd/ を見て
Makefile の `IVD_VER` を更新します。
(JSON は存在しない)

```
make SKK-JISYO.ivd
```

### 2.2.10. SKK-JISYO.lisp

直接編集します。(JSON は存在しない)

### 2.2.11. SKK-JISYO.mazegaki

SKK-JISYO.M から skk-mkmgk.el で生成、とのことですが古い情報のようです。
https://khiker.hatenablog.jp/entry/20101225/ddskk_mazegaki_dict
のような方法を検討すべきかもしれません。(あるいは廃止を検討)

### 2.2.12. zipcode

[zipcode/README.md](zipcode/README.md) を参照下さい。
(JSON は存在しない)


## 2.3. その他

noregist, not_wrong, wrong.annotated は直接編集します。
(JSON は存在しない)


# 3. 各辞書の編集方針について

## 3.1. 共通

一般名詞は、固有名詞より候補の優先度を上げる。

## 3.2. SKK-JISYO.S

- Palmtop など限定的な環境でも使えるようサイズを極力抑える。

- 全体のエントリ数が少ない分、単漢字を増やすが、頻度の低い単漢字の読み
  は外す。

- 誤った読みは採用しない。

- ~/.skk-jisyo に rename して個人辞書として利用できるよう、最低限の環
  境を整える。

### 3.2.1. 原則含めるもの

- 第一水準の区点の順序のもとになっている漢字 (頻度は低くても)

  第一水準でも、「喧嘩」の「嘩」とか、「林檎」の「檎」とか用例がほぼユ
  ニークな文字や固有名詞くらいしか使い道のなさそうな「莞」のような文字
  は特殊例でしょうが。

- 常用漢字の読みになっている漢字 (頻度は低くても)

### 3.2.2. 原則含めないもの

- 複合語。固有名詞、abbrev entry、数値変換エントリ

- JIS 規格外の文字

### 3.2.3. 候補の出現順序を決める「頻度」の定義

文字単位の頻度、つまり、別な単語に含まれる (同じよみの) 同じ字を勘定に
入れて数えます。こう数えると、新しい単語の登録がしやすくなります。

これだけではわかりづらいかもしれません。たとえば「けん /県/」というエ
ントリーの場合、「…県」という使われ方があります。

S 辞書ではそういう (「ちばけん /千葉県/」のような) 長いエントリーはな
いのが普通なので、将来、そういうエントリーが入力されるであろうことを考
えて、頻度にすればよいわけで、その分、「県」という字の重みが多少増しま
す。

L 辞書ではそういう長いエントリーもたぶん初めからあるので、利用者は普通
「ち」「ば」「けん」と別々に変換したりはせず、「ちばけん」を「千葉県」
に変換します。ですから、「県」という字が単独で使われる場合を数えて、頻
度にすればよいわけです。

### 3.2.4. 「正しい読み」「誤った読み」の基準

学問的なことはさておき、便利かどうか、が重要。

例えば漢和辞典的には「釧」は「くしろ」と読むのが「正し」くて、「くし」
と読むのは「誤り」のはずです。L 辞書には両方を登録すればいいわけですが、
S 辞書に両方登録する必要があるのかというと、「くし」だけで十分、という
判断もありえます (実際、この文字の場合、その後、用例を調べた上で、単漢
字の「釧」をはずし、「くしろ /釧路/」だけを登録してあります) 。

### 3.2.5. その他

- 同音異字を少なくしたい

  音読み偏重になっていて同音異字が多い「こう」のようなエントリーは訓読
  みに移したい。

  同じ読みの他の字がなければ、正しい読みは採用。あとは同音異字 (つまり
  候補) の数次第。たとえば、「こう」と読む字はもう増やしたくない、とか、
  「け」と読む字は 8つだけだから、正しいならやはり入れようかな、とか。

## 3.3. SKK-JISYO.M

- 既に登録されている文字について、明らかに間違いでない限り削除はしない。

- 単漢字を充実させる。S 辞書の成果を取り込むし、独自に追加することもあ
  る。

  一文字の単漢字でもM辞書になくてよさそうなエントリーだってあるでしょ
  う。古語のエントリーの多くはきっとそうです。「ぶんまわし /規/」とか、
  M 辞書にはいらない。

- 単漢字以外の追加登録は、よっぽどのことがない限り行わない。

- size 制限として 20 KB ぐらいを目安とする。

  SKK 本体と M 辞書を入れて FD 1 枚に収まるようにという主旨 (SKK 本体
  もどんどん大きくなるので大雑把な基準) 。

## 3.4. SKK-JISYO.L

### 3.4.1. 単純なかな → カナ、ascii → 全角英文字の変換エントリを含まない

既存のエントリは削除しました。そういうものを追加し始めるときりがない、
というのが追加拒否の理由です。そういう変換が必要な方にはプログラム側で
対応することにします。ruby/Migemo も kakasi もそのような変換は辞書に頼
らずプログラム側で対応しています。

但し、

- きつi /キツ/
- まずi /マズ/

というような送りありエントリは例外です。送りあり変換でこれらの入力を辞
書を使わずにすることができないからです。

また、

- adobe /Ａｄｏｂｅ/
- bluenote /ＢｌｕｅＮｏｔｅ/

のようなものも許容範囲とします。要するに SKK の他の変換方法で、一回で
変換できないものは許容範囲です。

### 3.4.2. annotation の積極的付加

# 4. ChangeLog の記述方法について

## 4.1. 真鵺道を使用する

SKK 辞書を修正して commit する際は必ず、skk/dic/ChangeLog をお書き下さ
い。

ChangeLog の記述方法は「真鵺道」(まぬえど) を使います。真鵺道について
詳しくは、

  - http://sundayresearch.eu/hitoshi/otherprojects/manued/index-j.html

  - https://github.com/yamauchih/manued

をご参照下さい。

真鵺道についてこの文章で簡単にご説明しますが、お願いしたいポイントを先
に書きますと、

  (1) 真鵺道を使って ChangeLog を書いて下さい。書き方が分からないとき
      は github issues でお尋ね下さい。

  (2) deforder は older-first (newer-last) で書いて下さい。

  (3) defcomment を使って修正した理由を簡単にコメントに書いて下さい。


真鵺道には簡単な文法があって、文法で使う記号はユーザが選択できるように
なっていますが、SKK の ChangeLog で書く標準文字はまだ決めていませんの
で、とりあえず自由に書いてもらって結構です。

但し、新旧のエントリ順を決める deforder だけは、older-first (newer-last)
で書いて下さい。つまり変更前のエントリを先に書き、修正後を後で書く手法
です。

このファイルではとりあえず、

```
  defLparenthesis "{"
  defRparenthesis "}"
  defswap "|"
  defdelete "->"
  defcomment ";"
  defescape "~"
```

のように仮決めしてご説明します。

## 4.2. 真鵺道を使った記述例

真鵺道を使った標記で最も簡単な例は次のようなものです (いずれも
ChangeLog での記述を真似て書いています)。

	* SKK-JISYO.L: designer /デ{サ->ザ}イナー/

defLparenthesis と defRparenthesis で囲まれた区間が真鵺道のコマンドの
対象です (この区間を以下「コマンド区間」と呼びます)。defdelete の前に
あるのが修正前、後にあるのが修正後です。

上記の例は、候補の「デサイナー」の内、2 文字目の「サ」を「ザ」に修正し
ています。

### 4.2.1. 候補を削除するとき

ある候補を削除するときは次のように書きます。

	* SKK-JISYO.edict: amalgamation /合同/合併/{并合/->}

### 4.2.2. 候補を追加するとき

ある候補を追加するときは次のように書きます。

	* SKK-JISYO.L: てきかく /的確/適格/{->適確/}

### 4.2.3. エントリ (見出しと候補のセットの 1 行) を削除するとき

あるエントリを削除するときは次のように書きます。

	* SKK-JISYO.S: {てい /袋/->}

### 4.2.4. エントリを追加するとき

逆に新たなエントリを追加するときは次のように書きます。

	* SKK-JISYO.L: {->まぬえど /真鵺道/Manued/Manuscript Editing/}

### 4.2.5. コメントの挿入

コマンド区間に defcomment を挿入することで次の真鵺道コマンドが出てくる
までの間にコメントを挿入することができます。

	* SKK-JISYO.edict: merger /合併/混同/{并合/->;広辞苑の第四版にも大
	辞林にもこんな言葉は載ってない。}

特に削除するときは、後の検証が楽になるように必ずコメントを挿入していた
だくことをお願いします。また、真鵺道だけでは表現できない修正にコメント
を利用することもできます。

複数のエントリに対し、同様の修正をするときは、例えば

	* SKK-JISYO.edict: merger /合併/混同/{并合/->;広辞苑の第四版にも大
	辞林にもこんな言葉は載ってない。}
	annexation /合体/合併/付加/併合/{并合/->;同上}
	amalgamation /合同/合併/{并合/->;同上}

のような形で「同上」など適当な言葉を使うことで重複したコメントを省略し
ていただいても結構です。人間が読むものなので、読み易い形が優先です。

以下コメントの例を二、三。

	* SKK-JISYO.L: てきかく /的確/適格/{->適確/;的確と同義。広辞苑の「的確」の項に載っていた。}

	* SKK-JISYO.edict: discrete /{細心;「細心」の意味はない->不連続の}/

こんな感じです。

### 4.2.6. SKK-JISYO.wrong.annotated で真鵺道する際の注意

manued-eval-last-manuexp したときに間違いの方を残すために、
manued-is-order-older-first を nil に設定していますが、実際の manued
command は older-first の順ですので、ご注意下さい。

# 5. 辞書の形式について。

## 5.1. 辞書の形式

どんな小さな辞書でも必ず「;; okuri-ari entries.」と
「;; okuri-nasi entries.」の２行を含んでいます。前者の次行から送り仮名
を含むエントリが並びます。後者の前行までが送り仮名「あり」のエントリで
す。このエントリの集りを「送りありエントリ」と呼んだりします。

後者の次行から最終行までが「送りなしエントリ」です。

各行は

  さい /際/差異/才/再/最/歳/

のように、「見出し語 /候補0/候補1/候補2/.../候補n/」というような形式に
なっています。候補は前にある程優先して出力されます。

## 5.2. エントリ

見出し語と候補を合わせた１行を便宜上「エントリ」と呼んでいます。上記
の例ですと、

  さい /際/差異/才/再/最/歳/

がひとつのエントリです。

## 5.3. 何故「送りありエントリ」と「送りなしエントリ」に分かれているか

SKK では送り仮名があるかないかをユーザ側で明示して変換しますので、送り
仮名ありの変換を行なった場合は、「送りありエントリ」を、そうでない場合
は「送りなしエントリ」を検索します。

## 5.4. ] だけの候補？

  おおk /大/多/[き/大/]/[く/多/大/]/

のようなエントリのことをおしゃっているのでしょう。

このようなエントリは個人辞書の送りありエントリでしか見られません。

~/.skk で skk-henkan-okuri-strictly を t にセットすると、検索の際、送り
仮名の厳密なマッチを要求するようになります。このとき、検索の手順は、お
およそ次のようになります。例えば、`OoKi' とタイプすると、

  (1) `おおk' を見出しとして個人辞書を検索。

  (2) 見つかったら、次は同一行に `[き' があるかどうか検索する (あった!)。

  (3) 見つかったら、次に現われる `]' までの間に挟まれている候補だけを
      出力する (「大」だ!)。

従って、「多」については、候補として含まれているにもかかわらず、出力さ
れません。

skk-henkan-okuri-strictly が nil の場合は、上記 (2), (3) のプロセスは
省略され、見出し語が見つかったら `[(送り仮名)' のような候補が出てくる
までの候補を全て出力します (上記の例ですと、「大」と「多」)。

skk-henkan-strict-okuri-precedence を t にセットすると、送り仮名がマッ
チするものを優先的に出力し、マッチしないものも劣後的に出力します。

## 5.5. 個人辞書の見出し語はソートされていない？

個人辞書は確定したもの順に並んでいます。比較的サイズが小さいことからリ
ニアサーチを前提にしていること (確定の度にきちんと見出し語をソートさせ
てバイナリサーチを行なうよりは効率が良いと判断しています)、また、見出
し語の補完で最近確定した見出しから順に出力するため、などの理由からこの
ような実装になっています。

## 5.6. SKK-JISYO.{SML} の送りありエントリの見出し語のソートが逆順になっている

なんででしょう？  分かりません (^^;;。

  (1) 昔の個人辞書は送り仮名の自動処理のために、送りありエントリのみ見
      出し語を逆順ソートさせていたが、その名残り (現在は skk-auto.el
      の内部処理でこれと同等の作業を行なっています)。

  (2) リニアサーチする際、「;; okuri-nasi entries.」の区切りを中心に、
      送りあり変換については上方に、送りなし変換については下方にサーチ
      することでポイント移動を最小限に抑えることができるから。

なんてとこかな、と想像しているのですが。

<!--
Local Variables:
  coding: utf-8
  mode: Markdown
end:
-->
