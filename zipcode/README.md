このディレクトリには郵便番号辞書を置いています。


# ファイル

 1. `SKK-JISYO.zipcode` … SKK 用の 7 桁郵便番号辞書です。

 2. `SKK-JISYO.office.zipcode` … SKK 用の事業所郵便番号辞書です。

 3. `words.zipcode` … これは郵便番号辞書を `skk-look` の機能と組み合わせて利用
   する場合に `/usr/dict/words` というファイルに書くべき内容が書かれています。

 4. `Makefile`, `ZIPCODE-MK` … これらは、日本郵便株式会社の「郵便番号データダ
   ウンロード」から取得できるファイルを基に、自分で郵便番号辞書を作る場合に使い
   ます（最新版に更新したいときなど）。Emacs 26 以上を使う必要があります。

  [日本郵便株式会社「郵便番号データダウンロード」](http://www.post.japanpost.jp/zipcode/download.html)

上記のうち、1, 2, 3 は public domain であり、4 については GPL のもとで配布され
ます。


# インストール

* `SKK-JISYO.zipcode` … `SKK-JISYO.L` に内容を追加する、などの方法で利用可能に
  します。`skk-tools` がインストールされているならば、

  ```
  % skkdic-expr SKK-JISYO.L + SKK-JISYO.zipcode | skkdic-sort > SKK-JISYO.LL
  ```

  のようにして新しい辞書を作れます。 一緒にしたくない場合は複数辞書に対応した辞
  書サーバを利用するのがいいでしょう。

* `SKK-JISYO.office.zipcode` … 同上。
 
* `words.zipcode` … `/usr/dict/words` のエントリに追加します。このとき、
  alphabet のエントリより上位に追加しないと正しく検索できないようです。

  ```
  % mv /usr/dict/words /usr/dict/words.original
  % sort -fd words.zipcode /usr/dict/words.original > /usr/dict/words
  ```

  とするなど、適当にインストールしてください。

なお，同梱の辞書を使うのではなく、最新の郵便番号データを自動的にダウンロードし
て辞書をつくり直すこともできます。それには

```
% sh configure
% make batch
```

を実行します。


# 使用法

* "/" で `skk-abbrev-mode` に入り、７桁の数字を入力すれば地名に変換できます。

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

この辞書を生成するにあたって、地名に関する知識が不足していること、情報量が膨大
なことなどにより、すべての地名について正確かどうかチェックすることはできていま
せん。

そこでよろしければ、よく御存知の地名について、間違っていないかどうか調べてみて
ください。もし間違いがありましたら、SKK 開発ラボ <skk@ring.gr.jp> にご連絡くだ
さい。


# メンテナンスの方法 (コミッタの方へ)

日本郵便提供のデータは基本的に月 1 回更新されます。それに伴う本辞書の更新は以下
の手順で行います。

## 手順

```
% cd $(somewhere)/skk/dic/zipcode
% cvs up
% sh configure
% make batch-update
```

(ここで生成された差分ファイルを確認して)

```
% cvs ci
```

なお、`make batch-update` には `wget` と `lha` が必要です。ない場合は

 http://www.post.japanpost.jp/zipcode/dl/kogaki/lzh/ken_all.lzh

 http://www.post.japanpost.jp/zipcode/dl/jigyosyo/lzh/jigyosyo.lzh

を取得して、適当な方法で展開したうえで

```
% make update
```

としてください。

## 差分ファイルについて

`SKK-JISYO.office.zipcode`, `SKK-JISYO.zipcode`, `words.zipcode` をチェックイン
する前に、これらの差分を必ず確認してください。これは以下のような理由によります。

* `ZIPCODE-MK` にバグがあると変なエントリを生成してしまう可能性がある。

* 日本郵便のデータの、書式が変わるなどの理由によって `ZIPCODE-MK` が対応できなく
  なっている可能性がある。

* 使用する Emacs によっては、元データの内容を正しくデコードできないなどの理由に
  よって、変なエントリを生成してしまう可能性がある。

## 必要なもの

* `skk-tools`

* Emacs … Emacs 22 以上。 XEmacs でも一応できますが、XEmacs 21.1 系では一部正し
  くデコードできないので、XEmacs 21.4.4 以上のものを使ってください。


# この辞書の出来、他の郵便番号辞書、などについて

世の中には郵便番号辞書が種々、存在します。同じデータを元にしているものが多いは
ずですが、その出来栄えには差があります。

日本郵便が配布しているデータは CSV 形式で、一応、一定の形式で書かれています。と
ころが、この「一応」というのがなかなかクセ者で、ひとすじなわではいきません。単
純なスクリプトで文字列を結合しただけだと、ヘンテコな地名がたくさんできてしまい
ます。

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
