;;; emoji.el ---   -*- coding: utf-8 -*-

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
;; ファイル https://unicode.org/emoji/charts-13.0/emoji-list.html を基に、
;; skk 辞書のエントリー形式を標準出力へ吐き出します。

;; emoji-list.html では、ひとつの emoji について、複数の Other Keywords が振られています。
;; また、ある Keyword は、複数の emoji のキーワードとして登録されています。
;; ここでは、Keyword を見出し語とし、emoji を候補としたエントリー形式を作成することとし
;; Short Name はアノテーションとしました。

;; 後続する skkdic-expr2 において、同じ見出し語はひとつのエントリーに集約されます。

;;; Code:

(defun text2jisyo ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "emoji-list.txt" "./"))
    (goto-char (point-min))
    (while (re-search-forward "title='\\(U\+[^ ]+ \\)+\\([^ ]+\\) \\(.+\\)' class=" nil t)
      (let ((emoji (match-string 2))
            (name (match-string 3)))
        (setq name (replace-regexp-in-string "flag: " "" name))
        (forward-line 2)
        (re-search-forward "<td class='name'>\\(.+\\)</td>")
        (dolist (midasi (split-string (match-string 1) " | "))
          ;; Keyword の１文字目が 0x7b 以下であれば、エントリー化する。
          ;; （Keyword に `得' などが存在するので）
          (when (< (string-to-char (substring midasi 0 1)) ?{)
            (setq midasi (replace-regexp-in-string "<span class='keye'>(?" "" midasi))
            (setq midasi (replace-regexp-in-string ")?</span>" "" midasi))
            (princ (format "%s /%s;%s/\n" (clear-str midasi) emoji name))))))))

(defun clear-str (midasi)
  ;; 見出し語は ascii 文字に限りたい（見出し語を作るための変換操作はイヤ）
  ;; Keyword に含まれる ` ' は `-' とする。
  (let ((lst '((8217 "'")               ; ’
               (8220 "")                ;
               (8221 "")                ; ”
               (232 "e")                ; yu è bǐng
               (234 "e")                ; cr ê pe
               (241 "n")                ; pi ñ ata
               (243 "o")                ; h ó ngbāo
               (257 "a")                ; hóngb ā o
               (333 "o")                ; gy ō za
               (464 "i")                ; yuèb ǐ ng
               (32 "-")
               )))
    (dolist (pair lst)
      (setq midasi (replace-regexp-in-string (char-to-string (car pair))
                                             (car (cdr pair))
                                             midasi))))
  midasi)

(defun check-midasi ()
  ;; 生成した SKK-JISYO.emoji の各エントリの見出し語をチェック
  ;; 見出し語に用いている文字は ascii の範囲かどうか
  (with-temp-buffer
    (insert-file-contents (expand-file-name "SKK-JISYO.emoji.tmp" "./"))
    (goto-char (point-min))
    (while (re-search-forward "^\\([^ ]+\\) .*$" nil t)
      (let ((midasi (match-string 0)))
        (dolist (c (mapcar #'(lambda (x)
                               (string-to-char x))
                           (split-string (match-string 1) "" t)))
          (when (< ?z c)
            (princ (format "%s : %s in %s\n" c (char-to-string c) midasi))))))))

(defun katakana (str)
  (let* ((lst (split-string str "" t))
         (d (- #x30a1 #x3041)))
    (mapconcat #'(lambda (s)
                   (let ((c (string-to-char s)))
                     (if (and (< #x30a0 c)
                              (< c #x30f7))
                         (char-to-string (- c d))
                       s)))
               lst "")))

(defun mk-alist (file)
  (let (alist)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (search-forward ";; okuri-nasi entries.")
      (forward-line)
      (while (not (eobp))
        (let* ((line (buffer-substring (point) (progn (end-of-line) (point))))
               (entry (split-string line " /"))
               (midasi (car entry))
               (first-cand (katakana (car (split-string (car (cdr entry)) "/" t)))))
          (setq alist (cons (cons midasi first-cand) alist)))
        (forward-line)))
    alist))

(defun edict-midasi ()
  (let ((alist (mk-alist (expand-file-name "SKK-JISYO.edict" "./"))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "SKK-JISYO.emoji.tmp" "./"))
      (goto-char (point-min))
      (forward-line 3)
      (while (not (eobp))
        (let* ((line (buffer-substring (point) (progn (end-of-line) (point))))
               (entry (split-string line " /"))
               (midasi (car entry))
               (cands (car (cdr entry)))
               (result (cdr (assoc midasi alist))))
          (when result
            (princ (format "%s /%s\n" result cands)))
          (forward-line))))))

(defun mk-allcands-alist (file)
  (let (alist)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (search-forward ";; okuri-nasi entries.")
      (forward-line)
      (while (not (eobp))
        (let* ((line (buffer-substring (point) (progn (end-of-line) (point))))
               (entry (split-string line " /"))
               (midasi (replace-regexp-in-string ">" "" (car entry)))
               (cands (split-string (car (cdr entry)) "/" t)))
          (dolist (cand cands)
            (setq alist (cons (cons cand midasi) alist))))
        (forward-line)))
    alist))

(defun l-midasi ()
  (let ((alist (mk-allcands-alist (expand-file-name "SKK-JISYO.L.unannotated" "./"))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "emoji-kanji.tmp" "./"))
      (goto-char (point-min))
      (forward-line 3)
      (while (not (eobp))
        (let* ((line (buffer-substring (point) (progn (end-of-line) (point))))
               (entry (split-string line " /"))
               (midasi (car entry))
               (cands (car (cdr entry)))
               (result (cdr (assoc midasi alist))))
          (when result
            (princ (format "%s /%s\n" result cands)))
          (forward-line))))))

(defun kana-p (str)
  "STR の構成がすべて kana なら t."
  (let ((result t)
        (lst (split-string str "" t)))
    (dolist (s lst)
      (unless (eq (aref char-script-table (string-to-char s))
                  'kana)
        (setq result nil)))
    result))

(defun midasi-kana-p ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "emoji-edict-matched.tmp" "./"))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((line (buffer-substring (point) (progn (end-of-line) (point))))
             (entry (split-string line " /"))
             (midasi (car entry)))
        (when (kana-p midasi)
          (princ (format "%s\n" line)))
        (forward-line)))))

(provide 'make-emoji)

;;; make-emoji.el ends here
