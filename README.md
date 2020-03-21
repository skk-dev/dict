# SKK dictionary files

Refer to the following URL for the distribution files.

https://skk-dev.github.io/dict/


# 辞書ファイルを編集する

[committers.md](committers.md) を参照してください。

# 配布用の gzip アーカイブを作る

`make archive` を実行すると、配布用の gzip ファイルを作ることができます。
この過程では `skktools` を用いて `csv/china_taiwan.csv` から `SKK-JISYO.china_taiwan` を
生成しているため、あらかじめ Ruby 処理系と `skktools` をインストールしておいてく
ださい。

具体的には、次のようにコマンドを実行すると、

```
$ make archive TOOLS_DIR=../github.skktools
```

カレントディレクトリに `SKK-JISYO.*.gz`, `SKK-JISYO.*.md5`, `zipdoce.*.gz`,
`zipdoce.*.md5` が生成されます。

これらのファイルは、いったん退避しておきましょう。

```
$ mv SKK-JISYO.*.gz ../
$ mv SKK-JISYO.*.md5 ../
$ mv zipdoce.*.gz ../
$ mv zipdoce.*.md5 ../
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

さきほど退避しておいた `SKK-JISYO.*.gz`, `SKK-JISYO.*.md5`, `zipdoce.*.gz`, `zipdoce.*.md5` を
ブランチ gh-pages に mv して add && push します。
