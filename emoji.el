;;; emoji.el --- generate EMOJI SKK-JISYO from CLDR annotations ja.xml -*- coding: utf-8 -*-

;;; Commentary:

;;  Unicode Common Locale Data Repository
;;    https://unicode.org/Public/cldr/36.1/cldr-common-36.1.zip
;;    common/annotations/*.xml

;;; License:
;;    https://www.unicode.org/license.html

;;; TODO:

;; (1) yuèbǐng /🥮/
;;     ASCII 範囲外の見出し語を適切に置き換える 

;; (2) お寿司 /🍣/
;;     漢字混じりの見出し語とならないよう（かな入力できない。補完もできない）
;;     出力していない。 validate2()
;;     L 辞書と突合して漢字をかなへ変換する

;; (3) 候補に skk アノテーションを付加する
;;     Short Name や U+9999 など

;;; Code:

(require 'dom)

(defun xml-to-jisyo (file)
  (let* ((xml-dom-tree (with-temp-buffer
                        (insert-file-contents file)
                        (libxml-parse-xml-region (point-min) (point-max))))
         (doms-anno (dom-by-tag xml-dom-tree 'annotation)))
    (mapc #'(lambda (dom-anno)
              (let ((annos (split-string (dom-text dom-anno) " | "))
                    (cp (dom-attr dom-anno 'cp)))
                (mapc #'(lambda (anno)
                          (and (validate anno)
                               (validate2 anno)
                               (princ (format "%s /%s/\n" (treat anno) cp))))
                      annos)))
          doms-anno)))

(defun validate (anno)
  ;; ひとつでも t なら nil、すべて nil なら t
  ;; 無視する見出し語を列記する ... validate() は nil を返す
  (not (or (string-match "\\s-" anno)   ; スペースを含む
           (string-match "/" anno)      ; 単独 `/'
           )))

(defun validate2 (anno)
  ;; 漢字等を含む文字列は、見出し語としては不適当 ... nil を返す
  (let* ((strings (split-string anno "" t))
         (lst (mapcar #'(lambda (c)
                          (aref char-script-table (string-to-char c)))
                     strings)))
    (not (or (member 'han lst)
             (member 'cjk-misc lst)
             (member 'symbol lst)
             ))))

(defun treat (str)
  (setq str (replace-regexp-in-string (char-to-string 8220) "" str))
  (setq str (replace-regexp-in-string (char-to-string 8221) "" str))

  ;; 片仮名を平仮名へ変換
  (let ((diff (- #x30a1 #x3041))
        (lst (split-string str "" t))
        c)
    (mapconcat #'(lambda (s)
                   (setq c (string-to-char s))
		   (if (and (<= #x30a1 c) (<= c #x30f6))
		     (char-to-string (- c diff))
		   (char-to-string c)))
               lst "")))

(defun ja ()
  (xml-to-jisyo "ja.xml"))

(defun en ()
  (xml-to-jisyo "en.xml"))


(provide 'emoji)

;;; emoji.el ends here
