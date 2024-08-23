#!/bin/sh
# コミットする前に真鵺道チェック (Public domain)
# sh manued.sh SKK-JISYO.* zipcode/SKK-JISYO.*

# 事前に PATH を通しておく必要がある
if ! command -v docdiff >/dev/null; then
  echo "docdiff をインストールしてください"
  return 1
fi
if ! command -v iconv >/dev/null; then
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
UTF_SRCS=" .edict2 .emoji .fullname .ivd zipcode/SKK-JISYO.zipcode zipcode/SKK-JISYO.office.zipcode"

while [ $# -ne 0 ]; do
  case $UTF_SRCS in
    (*" ${1#SKK-JISYO} "*) ENC=utf-8 ;;
    (**) ENC=euc-jisx0213 ;;
  esac

  OLDLINES=`mktemp`
  NEWLINES=`mktemp`
  git diff -U0 $COMPARED -- $1 | iconv -f $ENC -t utf-8 | sed \
    -e '1,4d' -e 's/^@.*$//' \
    -e '/^[+]/d' -e 's/^-\(.*\)$/\t\1\n/' > $OLDLINES
  git diff -U0 $COMPARED -- $1 | iconv -f $ENC -t utf-8 | sed \
    -e '1,4d' -e 's/^@.*$//' \
    -e '/^-/d' -e 's/^[+]\(.*\)$/\t\1\n/' > $NEWLINES

  if [ "`wc -l < $OLDLINES`" != "0" ] || [ "`wc -l < $NEWLINES`" != "0" ]; then
    echo "	* $1:"
    docdiff --char --format=user $OLDLINES $NEWLINES | sed \
      -e '/^$/d' \
      -e 's/^{->\t/\n\tAdd entry.\n\t/;/^}$/d' \
      -e 's/^{\t/\n\tRemove entry.\n\t/;/^->}$/d' \
      -e 's/^\(.*->.*\)$/\n\tModify entry.\n\1/'
    echo
  fi

  rm $OLDLINES $NEWLINES
  shift
done

rm ~/.docdiff/docdiff.conf
