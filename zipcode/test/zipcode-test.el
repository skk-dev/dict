;;; zipcode-test.el --- 郵便番号辞書テスト -*- mode: emacs-lisp; coding: utf-8; -*-

;; Copyright (C) 2020 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
;; Author: 2020 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program, If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)

;; ken_all.csv
;;   26102,"602  ","6020033","ｷｮｳﾄﾌ","ｷｮｳﾄｼｶﾐｷﾞｮｳｸ","ｲﾏﾃﾞｶﾞﾜﾁｮｳ","京都府",
;;   "京都市上京区","今出川町（烏丸通今出川上る、烏丸通今出川下る、今出川通烏丸西",0,0,0,0,0,0

;; ZIPCODE-MK
;;   mkdic-process-kyoto()

;; SKK-JISYO.zipcode
;; 6020033 /京都府京都市上京区烏丸通今出川上る今出川町
;;         /京都府京都市上京区烏丸通今出川下る今出川町
;;         /京都府京都市上京区今出川通烏丸西入今出川町
;;         /京都府京都市上京区今出川通室町東入今出川町/

(ert-deftest jisyo-zipcode/test1 ()
  (should
   (string-equal "京都府京都市上京区烏丸通今出川上る今出川町/京都府京都市上京区烏丸通今出川下る今出川町/京都府京都市上京区今出川通烏丸西入今出川町/京都府京都市上京区今出川通室町東入今出川町/"
                 (with-temp-buffer
                   (let ((large-file-warning-threshold 20000000)
	                 (coding-system-for-read 'utf-8))
                     (insert-file-contents (expand-file-name "SKK-JISYO.zipcode" "./"))
                     (goto-char (point-min))
                     (search-forward "6020033 /")
                     (buffer-substring (point)
                                       (progn (end-of-line) (point))))))))

;; 特に総務省である必然性はない。移転しないであろう事業所として選んでみただけ。
(ert-deftest jisyo-office-zipcode/test1 ()
  (should
   (string-equal "総務省 @ 東京都千代田区霞が関２丁目１−２/"
                 (with-temp-buffer
                   (let ((large-file-warning-threshold 20000000)
	                 (coding-system-for-read 'utf-8))
                     (insert-file-contents (expand-file-name "SKK-JISYO.office.zipcode" "./"))
                     (goto-char (point-min))
                     (search-forward "1008926 /")
                     (buffer-substring (point)
                                       (progn (end-of-line) (point))))))))

;;; zipcode-test.el ends here
