;;; zipcode-test.el --- —X•Ö”Ô†«‘ƒeƒXƒg -*- mode: emacs-lisp; coding: japanese-shift-jis-2004; -*-

;;; Commentary:

;;; Code:

(require 'ert)

;; ken_all.csv
;;   26102,"602  ","6020033","·®³ÄÌ","·®³Ä¼¶Ğ·Ş®³¸","²ÏÃŞ¶ŞÜÁ®³","‹“s•{",
;;   "‹“ssã‹‹æ","¡oì’¬i‰GŠÛ’Ê¡oìã‚éA‰GŠÛ’Ê¡oì‰º‚éA¡oì’Ê‰GŠÛ¼",0,0,0,0,0,0

;; ZIPCODE-MK
;;   mkdic-process-kyoto()

;; SKK-JISYO.zipcode
;; 6020033 /‹“s•{‹“ssã‹‹æ‰GŠÛ’Ê¡oìã‚é¡oì’¬
;;         /‹“s•{‹“ssã‹‹æ‰GŠÛ’Ê¡oì‰º‚é¡oì’¬
;;         /‹“s•{‹“ssã‹‹æ¡oì’Ê‰GŠÛ¼“ü¡oì’¬
;;         /‹“s•{‹“ssã‹‹æ¡oì’Êº’¬“Œ“ü¡oì’¬/

(ert-deftest jisyo-zipcode/test1 ()
  (should
   (string-equal "‹“s•{‹“ssã‹‹æ‰GŠÛ’Ê¡oìã‚é¡oì’¬/‹“s•{‹“ssã‹‹æ‰GŠÛ’Ê¡oì‰º‚é¡oì’¬/‹“s•{‹“ssã‹‹æ¡oì’Ê‰GŠÛ¼“ü¡oì’¬/‹“s•{‹“ssã‹‹æ¡oì’Êº’¬“Œ“ü¡oì’¬/"
                 (with-temp-buffer
                   (let ((large-file-warning-threshold 20000000)
	                 (coding-system-for-read 'euc-jp))
                     (insert-file-contents (expand-file-name "SKK-JISYO.zipcode" "./"))
                     (goto-char (point-min))
                     (search-forward "6020033 /")
                     (buffer-substring (point)
                                       (progn (end-of-line) (point))))))))

;; “Á‚É‘–±È‚Å‚ ‚é•K‘R«‚Í‚È‚¢BˆÚ“]‚µ‚È‚¢‚Å‚ ‚ë‚¤–‹ÆŠ‚Æ‚µ‚Ä‘I‚ñ‚Å‚İ‚½‚¾‚¯B
(ert-deftest jisyo-office-zipcode/test1 ()
  (should
   (string-equal "‘–±È @ “Œ‹“sç‘ã“c‹æ‰à‚ªŠÖ‚Q’š–Ú‚P|‚Q/"
                 (with-temp-buffer
                   (let ((large-file-warning-threshold 20000000)
	                 (coding-system-for-read 'euc-jp))
                     (insert-file-contents (expand-file-name "SKK-JISYO.office.zipcode" "./"))
                     (goto-char (point-min))
                     (search-forward "1008926 /")
                     (buffer-substring (point)
                                       (progn (end-of-line) (point))))))))

;;; zipcode-test.el ends here
