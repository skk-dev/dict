#!/bin/sh

# コミットする前に真鵺道チェック (Public domain)
# sh manued.sh SKK-JISYO.*

if ! command -v docdiff >/dev/null; then
  echo "docdiff をインストールしてください"
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

while [ "$#" -ne 0 ]; do
  OLDLINES=`mktemp`
  git diff -U0 $1 | sed -n '1,3d;s/^@.*$//p;s/^[-]/\t/p' > $OLDLINES
  NEWLINES=`mktemp`
  git diff -U0 $1 | sed -n '1,3d;s/^@.*$//p;s/^[+]/\t/p' > $NEWLINES

  echo "	* $1: Modify entry."
  docdiff --char --format=user $OLDLINES $NEWLINES | sed '/^$/d'
  echo

  rm $OLDLINES $NEWLINES
  shift
done

rm ~/.docdiff/docdiff.conf

