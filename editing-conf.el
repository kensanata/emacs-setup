(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun increment-numbers-in-region (start end)
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "[0123456789]+" end t)
    (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))))

;; Add FIXME highlighting everywhere

(defun highlight-fixme ()
  (font-lock-add-keywords nil'(("\\<\\(FIXME!?\\)"
				1 font-lock-warning-face prepend))))

(add-hook 'find-file-hook 'highlight-fixme t)
