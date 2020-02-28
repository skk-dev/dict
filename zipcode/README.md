このディレクトリには郵便番号辞書を置いています。


# ファイル

 1. `SKK-JISYO.zipcode` … SKK 用の７桁郵便番号辞書です。public domain です。

 2. `SKK-JISYO.office.zipcode` … SKK 用の事業所郵便番号辞書です。public domain です。

 3. `words.zipcode` … これは郵便番号辞書を `skk-look` の機能と組み合わせて利用
   する場合に `/usr/dict/words` というファイルに書くべき内容が書かれています。
   public domain です。

 4. `Makefile`, `ZIPCODE-MK` … これらは、[日本郵便株式会社](https://www.post.japanpost.jp/) の
   [郵便番号データダウンロード](https://www.post.japanpost.jp/zipcode/download.html) か
   ら取得できるファイルを基に、自分で郵便番号辞書を最新版に更新したいときなどに使
   います。 GNU Emacs 26 以上を使う必要があります。GPL です。


# インストール

* `SKK-JISYO.zipcode` … `SKK-JISYO.L` など既存の辞書ファイルに内容を追加する、な
  どの方法で利用可能にします。[`skk-tools`](https://github.com/skk-dev/skktools) が
  インストールされているならば、

  ```
  % skkdic-expr SKK-JISYO.L + SKK-JISYO.zipcode | skkdic-sort > SKK-JISYO.LL
  ```

  のように実行することで、新しい辞書ファイルを作ることができます。
  
  内容を合成したくない場合は、複数辞書に対応した辞書サーバを利用するのがいいでし
  ょう。

* `SKK-JISYO.office.zipcode` … 同上。
 
* `words.zipcode` … `/usr/dict/words` のエントリに追加します。このとき、
  alphabet のエントリよりも上位に追加しないと正しく検索できないようです。

  ```
  % mv /usr/dict/words /usr/dict/words.original
  % sort -fd words.zipcode /usr/dict/words.original > /usr/dict/words
  ```

  とするなど、適当にインストールしてください。

なお，最新の郵便番号データを自動的にダウンロードして郵便番号辞書ファイル
（`SKK-JISYO.zipcode`, `SKK-JISYO.office.zipcode`, `words.zipcode`）を作り直すこと
もできます。それには

```
% sh configure
% make batch
```

を実行します。カレントディレクトリにある `SKK-JISYO.zipcode`, `SKK-JISYO.office.zipcode`,
`words.zipcode` が更新されます。


# DDSKK で郵便番号辞書ファイルを使ってみる

* <kbd>/</kbd> の打鍵で `skk-abbrev-mode` に入り、７桁の数字を入力すれば地名に変換できます。

* `skk-look` と組み合わせることで、５桁 or ３桁の郵便番号にも対応できます。
  `~/.skk` に

  ```
  (setq skk-use-look t)
  (setq skk-look-recursive-search t)
  ```

  と書きます。 `skk-abbrev-mode` で

  ```
  ▽390*
  ```

  と入力し、スペースキーを連打すると、上３桁 が 390 である郵便番号と地名が次々
  に候補に現れます。

  * 注1) このときウッカリ確定すると、 次回変換時に郵便番号と地名の対応が崩れま
  す（個人辞書の内容が優先されるため）。 `~/.skk` に以下のような設定をすること
  でこの問題は回避可能です。

  ```
  ;; `skk-abbrev-mode' で skk-look を使った検索をしたときに確定情報を
  ;; 個人辞書に記録しないようにする
  (add-hook 'skk-search-excluding-word-pattern-function
	    ;; KAKUTEI-WORD を引数にしてコールされるので、不要でも引数を取る
	    ;; 必要あり
	    #'(lambda (kakutei-word)
	        (and skk-abbrev-mode
		     (save-match-data
		       ;; `skk-henkan-key' が "*" で終わるとき
		       (or (string-match "\\*$" skk-henkan-key)
		           (string-match "^[0-9]*$" skk-henkan-key))))))
  ```

  * 注2) もし変換がうまくいかなかったら以下のパッチを当ててみてください（最近の
    Daredevil SKK には適用済み。）

  ```
  --- skk-look.el.orig    Fri Jul 14 18:06:13 2000
  +++ skk-look.el	Wed Aug  2 18:02:29 2000
  @@ -130,7 +130,7 @@
                        (delete '(skk-look) (copy-sequence skk-search-prog-list))))
                   (setq skk-henkan-key (car v))
                   (while skk-current-search-prog-list
  -                  (setq v3 (skk-search)
  +                  (setq v3 (let (skk-use-numeric-conversion) (skk-search))
                           v2 (if (not skk-look-expanded-word-only)
                                  (skk-nunion v2 (cons (car v) v3))
                                (if v3
  ```


# 使用者の皆さんへのお願い

この辞書ファイルを生成するにあたって、地名に関する知識が不足していること、情報
量が膨大なことなどにより、すべての地名について正確かどうかチェックすることはで
きていません。

そこでよろしければ、よく御存知の地名について、間違っていないかどうか調べてみて
ください。もし間違いがありましたら、SKK 開発ラボ <skk@ring.gr.jp> にご連絡くだ
さい。


# メンテナンスの方法 (コミッタの方へ)

日本郵便株式会社が提供しているデータは基本的に月１回更新されます。それに伴う本
辞書の更新は以下の手順で行います。

## 手順

```
% cd $(somewhere)/skk/dic/zipcode
% git pull
% ./configure
% make batch-update
% make test
```

この手順で生成された差分ファイル `*.diff` を確認してから

```
% git add -u && git commit -m "update zipcode"
```

なお、`make batch-update` を実行するには `wget` と `unzip` が必要です。

## 差分ファイルについて

`SKK-JISYO.office.zipcode`, `SKK-JISYO.zipcode`, `words.zipcode` を commit
する前に、これらの差分 `*.diff` を必ず確認してください。これは以下のような理由によ
ります。

* `ZIPCODE-MK` にバグがあると、変なエントリを生成してしまう可能性がある。

* 日本郵便株式会社のデータの書式が変わるなどの理由によって `ZIPCODE-MK` が対応で
  きなくなっている可能性がある。

* 使用する GNU Emacs によっては、元データの内容を正しくデコードできないなどの理由に
  よって、変なエントリを生成してしまう可能性がある。

## 必要なもの

* `skk-tools` https://github.com/skk-dev/skktools

* GNU Emacs 26 以上

# この辞書の出来、他の郵便番号辞書、などについて

世の中には郵便番号辞書が種々、存在します。同じデータを元にしているものが多いは
ずですが、その出来栄えには差があります。

日本郵便株式会社が配布しているデータは CSV 形式で、一応、一定の形式で書かれてい
ます。ところが、この「一応」というのがなかなかクセ者で、ひとすじなわではいきま
せん。単純なスクリプトで文字列を結合しただけだと、ヘンテコな地名がたくさんでき
てしまいます。

まず、郵便番号簿というものは、基本的には地名でなく郵便番号を調べるものだという
ことがあると思います。だから、「〜一円」、「〜市（その他）」のように、要らない
記述が入っているものが多くあります。これは取り除かなければなりません。

また、ひとつの郵便番号に関する記述が２行以上にわたっている場合があります。これ
はまず２行の内容を連結してから処理しないと、かなり楽しい地名ができあがってしま
います。

こうしたことを考慮すれば、だいたい使える地名ができますが、それでも一部の地域に
ついては悩まされます。例えば、京都市。次のような記述があります。

    〒6020033 京都府 京都市上京区 今出川町（烏丸通今出川上る、烏丸通今出川下る、
                                        今出川通烏丸西入、今出川通室町東入）

京都市の地名を知らないと、括弧内の情報をどう処理してよいのか途方にくれます。
少し調べた限りでは、これは

    〒6020033 京都府 京都市上京区 烏丸通今出川上る今出川町
                   〃           烏丸通今出川下る今出川町
                   〃           今出川通烏丸西入今出川町
                   〃           今出川通室町東入今出川町

と解釈するのが正しいように思われました。この手の「解釈」を必要とする記述が少な
くないので、なかなか機械的に作るというわけにはいきませんでした。

知識がないため解釈しきれない情報については、あえて切り捨てた場合もあります。
今後の課題は、こうした解釈の間違いを修正することが中心となります。


Windows 用などの郵便番号辞書をいくつか参考にしました。

* MS-IME, Japanist の辞書は、まずまず使えるレベルのようで、SKK用辞書はこの辺と
  同程度を目標にしています。

* VJE-Delta 2.5β の Linux 版に付いていた辞書は、今ひとつでした。

* WXG は、参考にしたなかではとくに充実しています。おそらく最強でしょう。下手を
  すると地元住民ですら知らないような、細かい地名まで出てきます。もっとも、これ
  だと逆に郵便用には詳しすぎて、かえって使いにくいかもしれませんが、やはり資料
  としてはたいへん良いものだと思います。

* Canna 用辞書

  ftp://ftp.debian.or.jp:/debian-jp/pool/main/c/canna-yubin/

  これは、単純に結合されたものです。京都市の地名などはおかしくなっています。

  http://bonobo.gnome.gr.jp/~nakai/canna/

  こちらの方が良い辞書です。全角英数字を排除してあるそうです。

* Wnn 用辞書

  http://www2.crl.go.jp/jt/jt221/tfujii/Postal/

  これは、単純に結合されたものです。京都市の地名などはおかしくなっています。
