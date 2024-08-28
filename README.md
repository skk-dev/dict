# SKK dictionary files

Refer to the following URL for the distribution files.

https://skk-dev.github.io/dict/


# 辞書ファイルの編集をお考えの方へ

[committers.md](committers.md) には、次の内容を掲載してあります。

- 各辞書ファイルに適用しているライセンス
- 各辞書の編集方針
- ChangeLog の記述方法
- 辞書の形式

いずれも、SKK 辞書ファイルの編纂に向け役立つ内容ですので、ぜひご確認ください。


# 配布用の gzip アーカイブを作る

`make archive` を実行すると、配布用の gzip ファイルを作ることができます。
この過程では `skktools` を用いて `csv/china_taiwan.csv` から `SKK-JISYO.china_taiwan` を
生成しているため、あらかじめ Ruby 処理系と `skktools` をインストールしておいてく
ださい。

具体的には、次のようにコマンドを実行すると、

```
$ make archive TOOLS_DIR=../github.skktools
```

カレントディレクトリに `SKK-JISYO.*.gz`, `SKK-JISYO.*.md5`, `zipcode.*.gz`,
`zipcode.*.md5` が生成されます。

これらのファイルは、いったん退避しておきましょう。

```
$ mv SKK-JISYO.*.gz ../
$ mv SKK-JISYO.*.md5 ../
$ mv zipcode.*.gz ../
$ mv zipcode.*.md5 ../
```


## make の副作用

`make archive` の実行過程で、いくつかのファイルが更新される場合があります。

```
$ git status

On branch master
Your branch is up to date with 'origin/master'.

Changes not staged for commit:
        modified:   SKK-JISYO.L.unannotated
        modified:   SKK-JISYO.wrong
```

更新内容に問題がなければ、そのまま commit してください。

```
$ git add -u && git commit
```


## 配布用の gzip アーカイブをブランチ gh-pages に配置

さきほど退避しておいた `SKK-JISYO.*.gz`, `SKK-JISYO.*.md5`, `zipcode.*.gz`, `zipcode.*.md5` を
ブランチ gh-pages に mv して add && push します。

```
$ git co gh-pages
$ mv ../SKK-JISYO.*.gz .
$ mv ../SKK-JISYO.*.md5 .
$ mv ../zipcode.*.gz .
$ mv ../zipcode.*.md5 .
$ git add -u && git commit
$ git push
```


# JSON 形式の辞書について

辞書データを JSON に変換したテキストファイルです。
一部の IME では JSON 形式の辞書をサポートしています。
JSON ファイルを生成するためには `make json` を実行してください。
このとき、スクリプト実行環境として Deno が必要です。
