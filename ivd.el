;;; ivd.el --- generate IVD SKK-JISYO from Unicode -*- coding: utf-8 -*-

;;; Commentary:

 ;; About the IVD, see UTS #37:
 ;;   http://www.unicode.org/reports/tr37/

;;; License:
;;    https://www.unicode.org/license.html


;;; Code:

(defun get-string (s)
     (char-to-string (string-to-number s 16)))

(defun make-ivd-jisyo ()
  (with-temp-buffer
    (insert-file-contents "IVD_Sequences.txt")
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (replace-regexp-in-string ";" "" (buffer-substring (point)
                                                                     (progn
                                                                       (end-of-line)
                                                                       (point))))))
        (when (not (string-match "#" line))
          (let* ((data (split-string line " "))
                 (base (get-string (nth 0 data)))
                 (ivs (format "%s%s" base (get-string (nth 1 data))))
                 (reg (nth 2 data))
                 (code (nth 3 data)))
            (princ (format "%s /%s;%s(%s)/\n" base ivs reg code)))))
      (forward-line))))

(provide 'ivd)

;;; ivd.el ends here
