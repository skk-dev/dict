;; -*- mode: fundamental; coding: euc-jp -*-
;; Lisp dictionary for SKK system
;; Copyright (C) 1988-1995, 1997, 1999-2010, 2014-2015
;;
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Hironobu Takahashi <takahasi@tiny.or.jp>,
;; Masahiro Doteguchi, Miki Inooka,
;; Yukiyoshi Kameyama <kameyama@kuis.kyoto-u.ac.jp>,
;; Akihiko Sasaki, Dai Ando, Junichi Okukawa,
;; Katsushi Sato and Nobuhiro Yamagishi
;; NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; MITA Yuusuke <clefs@mail.goo.ne.jp>
;; Tsuyoshi Kitamoto <tsuyoshi.kitamoto@gmail.com>
;; SASAKI Nobuyuki <nathancorvussolis@gmail.com>
;; SKK Development Team <skk@ring.gr.jp>
;;
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: SKK-JISYO.lisp,v 1.5 2016/10/06 12:02:21 skk-cvs Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2016/10/06 12:02:21 $
;;
;; This dictionary is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This dictionary is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; SKK-JISYO.L から concat 関数を除く Lisp の関数の候補を抜き出し
;; skk-relative-date 関数を追加した辞書です。
;;
;; concat 関数を含む候補は、SKK 辞書形式における候補の区切りである "/" や
;; 候補と注釈の区切りである ";" をエスケープする目的で使用されていると
;; 考えられる為 SKK-JISYO.L に残しています。
;;
;; プログラム実行変換で用いられる Lisp の関数を拡張する際はこの辞書を
;; 更新していって頂ければと思います。
;;
;; okuri-ari entries.
;; okuri-nasi entries.
#feet /(skk-gadget-units-conversion "feet" (string-to-number (car skk-num-list)) "cm")/
#inch /(skk-gadget-units-conversion "inch" (string-to-number (car skk-num-list)) "cm")/(skk-gadget-units-conversion "inch" (string-to-number (car skk-num-list)) "feet")/
#mile /(skk-gadget-units-conversion "mile" (string-to-number (car skk-num-list)) "km")/(skk-gadget-units-conversion "mile" (string-to-number (car skk-num-list)) "yard")/
#x# /(skk-times)/
#yard /(skk-gadget-units-conversion "yard" (string-to-number (car skk-num-list)) "cm")/(skk-gadget-units-conversion "yard" (string-to-number (car skk-num-list)) "feet")/
#かげつご /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 nil 0 0 0)) nil nil :mm #0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 'gengo 0 0 0)) nil nil :mm #0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 3 'gengo 0 0 0)) nil nil :mm #0)/
#かげつまえ /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 nil 0 0 0)) nil nil :mm -#0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 'gengo 0 0 0)) nil nil :mm -#0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 3 'gengo 0 0 0)) nil nil :mm -#0)/
#にちご /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 nil 0 0 0)) nil nil :dd #0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd #0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd #0)/
#にちまえ /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 nil 0 0 0)) nil nil :dd -#0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd -#0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd -#0)/
#ねんご /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 nil 0 0 0)) nil nil :yy #0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 'gengo 0 0 0)) nil nil :yy #0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 3 'gengo 0 0 0)) nil nil :yy #0)/
#ねんまえ /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 nil 0 0 0)) nil nil :yy -#0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 'gengo 0 0 0)) nil nil :yy -#0)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 3 'gengo 0 0 0)) nil nil :yy -#0)/
bar /(make-string (- fill-column 1) ?-)/
line /(make-string (- (window-width) 5) ?-)/(make-string (- (window-width) 5) (string-to-char comment-start))/
mail /(symbol-value 'user-mail-address)/
name /(symbol-value 'user-full-name)/
now /(current-time-string)/(substring (current-time-string) 11 16)/(substring (current-time-string) 11 19)/
pwd /(pwd)/
skk /(skk-version)/
time /(current-time-string)/
today /(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)))/(skk-current-date)/
tomorrow /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd 1)/(skk-relative-date nil nil nil :dd 1)/
version /(skk-version)/
yesterday /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)) nil nil :dd -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)) nil nil :dd -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd -1)/(skk-relative-date nil nil nil :dd -1)/
あさって /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd 2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)) nil nil :dd 2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)) nil nil :dd 2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd 2)/(skk-relative-date nil nil nil :dd 2)/
あした /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd 1)/(skk-relative-date nil nil nil :dd 1)/
あす /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)) nil nil :dd 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd 1)/(skk-relative-date nil nil nil :dd 1)/
おとつい /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)) nil nil :dd -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)) nil nil :dd -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd -2)/(skk-relative-date nil nil nil :dd -2)/
おととい /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)) nil nil :dd -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)) nil nil :dd -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd -2)/(skk-relative-date nil nil nil :dd -2)/
おととし /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 nil 0 0 0)) nil nil :yy -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 'gengo 0 0 0)) nil nil :yy -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 3 'gengo 0 0 0)) nil nil :yy -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 1 'gengo 0 0 0)) nil nil :yy -2)/
きのう /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)) nil nil :dd -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)) nil nil :dd -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)) nil nil :dd -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)) nil nil :dd -1)/(skk-relative-date nil nil nil :dd -1)/
きょう /(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 0 0 0)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s-%s-%s(%s)" 0 nil 0 0 nil)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 0 'gengo 1 0 0)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information nil 3 'gengo 0 0 0)))/(skk-current-date)/
きょねん /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 nil 0 0 0)) nil nil :yy -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 'gengo 0 0 0)) nil nil :yy -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 3 'gengo 0 0 0)) nil nil :yy -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 1 'gengo 0 0 0)) nil nil :yy -1)/
ことし /(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 nil 0 0 0)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 'gengo 0 0 0)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 3 'gengo 0 0 0)))/(skk-current-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 1 'gengo 0 0 0)))/
さくねん /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 nil 0 0 0)) nil nil :yy -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 'gengo 0 0 0)) nil nil :yy -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 3 'gengo 0 0 0)) nil nil :yy -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 1 'gengo 0 0 0)) nil nil :yy -1)/
さらいげつ /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 nil 0 0 0)) nil nil :mm 2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 'gengo 0 0 0)) nil nil :mm 2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 3 'gengo 0 0 0)) nil nil :mm 2)/
さらいねん /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 nil 0 0 0)) nil nil :yy 2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 'gengo 0 0 0)) nil nil :yy 2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 3 'gengo 0 0 0)) nil nil :yy 2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 1 'gengo 0 0 0)) nil nil :yy 2)/
しょうわ#ねん /(skk-gengo-to-ad "" "年")/(skk-gengo-to-ad "西暦" "年")/
せいれき#ねん /(skk-ad-to-gengo 0 nil "年")/(skk-ad-to-gengo 1 nil "年")/
せんげつ /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 nil 0 0 0)) nil nil :mm -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 'gengo 0 0 0)) nil nil :mm -1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 3 'gengo 0 0 0)) nil nil :mm -1)/
せんせんげつ /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 nil 0 0 0)) nil nil :mm -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 'gengo 0 0 0)) nil nil :mm -2)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 3 'gengo 0 0 0)) nil nil :mm -2)/
たいしょう#ねん /(skk-gengo-to-ad "" "年")/(skk-gengo-to-ad "西暦" "年")/
ばーじょん /(skk-version)/
へいせい#ねん /(skk-gengo-to-ad "" "年")/(skk-gengo-to-ad "西暦" "年")/
めいじ#ねん /(skk-gengo-to-ad "" "年")/(skk-gengo-to-ad "西暦" "年")/
らいげつ /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 nil 0 0 0)) nil nil :mm 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 0 'gengo 0 0 0)) nil nil :mm 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年%s月" 3 'gengo 0 0 0)) nil nil :mm 1)/
らいねん /(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 nil 0 0 0)) nil nil :yy 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 0 'gengo 0 0 0)) nil nil :yy 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 3 'gengo 0 0 0)) nil nil :yy 1)/(skk-relative-date (lambda (date-information format gengo and-time) (skk-default-current-date date-information "%s年" 1 'gengo 0 0 0)) nil nil :yy 1)/
