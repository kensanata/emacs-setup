;; Convenience stuff for working with Java source code and SQL
;; statements

(add-hook 'java-mode-hook 'asc:java-init)

(defun asc:java-init ()
  (setq indent-tabs-mode nil
	c-basic-offset 2))

(defun asc:sql-quote (start end)
  (interactive "r")
  (setq end (copy-marker end))
  (goto-char end)
  (insert "'")
  (goto-char start)
  (save-excursion
    (while (re-search-forward "[0-9][0-9]+" end t)
      (replace-match "?")))
  (save-excursion
    (while (search-forward "'" end t)
      (replace-match "''")))
  (save-excursion
    (while (search-forward "\n" end t)
      (replace-match "'\n||' ")))
  (insert "'"))

(defun asc:sql-quote-for-java (start end)
  (interactive "r")
  (setq end (copy-marker end))
  (goto-char end)
  (insert "\"")
  (set-mark (point))
  (goto-char start)
  (save-excursion
    (while (search-forward "\n" end t)
      (replace-match " \" +\n\"")))
  (insert "\"")
  (forward-char -1)
  (untabify start end))

(defun asc:sql-unquote-from-java (start end)
  (interactive "r")
  (setq end (copy-marker end))
  (goto-char end)
  (set-mark (point))
  (goto-char start)
  (save-excursion
    (while (re-search-forward "\\(^\\s-*\\(\\+\\s-*\\)?\"\\|\\s-*\"\\s-*\\(\\+\\s-*\\)?$\\)" end t)
      (replace-match ""))))

(defun asc:code-quote-for-java (start end)
  (interactive "r")
  (setq end (copy-marker end))
  (goto-char end)
  (insert "\"")
  (goto-char start)
  (save-excursion
    (while (search-forward "\"" end t)
      (replace-match "\\\\\"")))
  (save-excursion
    (while (search-forward "\n" end t)
      (replace-match "\\\\n\" +\n\"")))
  (insert "\"")
  (untabify start end))

(defun asc:sql-in-java-rewrite-plus (start end)
  (interactive "r")
  (setq end (copy-marker end))
  (goto-char start)
  (save-excursion
    (while (re-search-forward "\"\\s-*\\+\n\\(\\s-*\\)\"" end t)
      (replace-match "\"\n\\1+ \""))))
