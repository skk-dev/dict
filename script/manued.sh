#!/bin/sh
# コミットする前に真鵺道チェック (Public domain)
# sh manued.sh SKK-JISYO.* zipcode/SKK-JISYO.*

THISFILE=`command -v -- "$0"`
THISDIR=`dirname -- "$THISFILE"`

# 事前に PATH を通しておく必要がある
if ! command -v deno > /dev/null; then
  echo "deno をインストールしてください"
  return 1
fi
if ! command -v docdiff > /dev/null; then
  echo "docdiff をインストールしてください"
  return 1
fi
if ! command -v iconv > /dev/null; then
  echo "iconv をインストールしてください"
  return 1
fi

# docdiff のユーザータグはコマンドラインで指定できない
# https://github.com/hisashim/docdiff/pull/29
if [ -f ~/.docdiff/docdiff.conf ]; then
  echo "docdiff 設定ファイルが存在しています"
  echo "~/.docdiff/docdiff.conf を消去してください"
  return 1
fi
mkdir -p ~/.docdiff
cat > ~/.docdiff/docdiff.conf <<EOF
tag_del_start           = {
tag_del_end             = ->}
tag_add_start           = {->
tag_add_end             = }
tag_change_before_start = {
tag_change_before_end   = ->
tag_change_after_end    = }
EOF

# 名前とメアドは環境変数でもセットできる
echo "`date +%Y-%m-%d`  ${USERNAME:-`git config user.name`}  <${USEREMAIL:-`git config user.email`}>"
echo

# HEAD^..HEAD とかにすると便利な場合もある
COMPARED=${COMPARED:-HEAD}

# Makefile 参照
UTF_SRCS=" .edict2 .emoji .fullname .ivd zipcode/SKK-JISYO.zipcode zipcode/SKK-JISYO.office.zipcode "

# デフォルトでチェックするファイル
# edict2 と ivd は自動更新で大量に出るので除外
if [ $# -eq 0 ]; then
  OTHER_SRCS=" .assoc .china_taiwan .edict .geo .hukugougo .itaiji .itaiji.JIS3_4 .jinmei .JIS2 .JIS2004 .JIS3_4 .law .lisp .L .mazegaki .M .ML .okinawa .propernoun .pubdic+ .requested .S .station .wrong.annotated "
  DEFAULT_SRCS="`echo ' '$UTF_SRCS $OTHER_SRCS | sed 's/ \.\(edict2\|ivd\) / /g' | sed 's/ \./ SKK-JISYO./g'`"
  set $DEFAULT_SRCS
fi

# git diff の -I オプションは 2.30 以降にしかない
# https://github.com/git/git/commit/296d4a94e7231a1d57356889f51bff57a1a3c5a1
# 古い git では "fatal: bad revision" エラーになる
(echo "bad version 2.29.4"; git --version) \
| sort -Vk3 | tail -1 | grep git > /dev/null && \
IGNORECOMMENTS="-I '^;'"

while [ $# -ne 0 ]; do
  case $UTF_SRCS in
    (*" ${1#SKK-JISYO} "*) ENC=utf-8 ;;
    (**) ENC=euc-jisx0213 ;;
  esac

  OLDLINES=`mktemp`
  NEWLINES=`mktemp`
  ( \
    git diff $IGNORECOMMENTS -U0 $COMPARED -- $1 \
    | iconv -f $ENC -t utf-8 \
    | sed -e '1,4d' \
    | tee /dev/stderr \
    | sed -e '/^[+@]/d' \
    > $OLDLINES \
  ) 2>&1 \
  | sed -e '/^[-@]/d' \
  > $NEWLINES

  if [ "`wc -l < $OLDLINES`" != "0" ] || [ "`wc -l < $NEWLINES`" != "0" ]; then
    echo "	* $1:"
    deno --allow-read --allow-write "$THISDIR/manued-helper.ts" \
      --old $OLDLINES --new $NEWLINES
    docdiff --char --format=user $OLDLINES $NEWLINES
    echo
    echo
  fi

  rm $OLDLINES $NEWLINES
  shift
done

rm ~/.docdiff/docdiff.conf
