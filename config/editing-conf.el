(defun increment-number-at-point (&optional num)
  "Increment number at point by 1.
Works with numerical arguments, too.
With a negative argument (just M--), uses -1.
With a universal argument (just C-u), ask by how much."
  (interactive "P")
  (save-excursion
    (when (zerop (skip-chars-backward "-0123456789."))
      (skip-syntax-forward "-"))
    (or (looking-at "-?[0123456789.]+")
	(error "No number at point"))
    (cond ((null num)
	   (setq num 1))
	  ((eq num '-)
	   (setq num -1))
	  ((listp num)
	   (setq num (read-number "Increment by how much? " 1))))
    (replace-match (number-to-string (+ num (string-to-number (match-string 0)))))))

(global-set-key (kbd "C-+") 'increment-number-at-point)

(defun increment-numbers-in-region (start end)
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "" end t)
    (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))))

;; Add FIXME highlighting everywhere

(defun highlight-fixme ()
  (font-lock-add-keywords nil'(("\\<\\(FIXME!?\\)"
				1 font-lock-warning-face prepend))))

(add-hook 'find-file-hook 'highlight-fixme t)
