;; The text files in my Dropbox folder are Markdown files.
(add-to-list 'auto-mode-alist '("/Dropbox/.*\\.txt\\'" . markdown-mode))

;; The Markdown files I write using IA Writer use newlines to separate
;; paragraphs. That's why I need Visual Line Mode. I also need to
;; disable M-q. If I fill paragraphs, that introduces unwanted
;; newlines.
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'as/markdown-config)

(defun as/markdown-config ()
  (local-set-key (kbd "M-q") 'ignore))

;; I write a lot of Markdown but then I want to post the text on
;; Google+ so here's a quick export.
(defun as/markdown-region-to-google (start end)
  (interactive "r")
  (goto-char start)
  (while (search-forward "*" end t)
    (goto-char (match-beginning 0))
    (cond ((looking-at "\\b\\*\\*\\|\\*\\*\\b")
	   (delete-char 1)
	   (forward-char 1))
	  ((looking-at "\\b\\*\\|\\*\\b")
	   (delete-char 1)
	   (insert "_")))))

;; Often Markdown gets added to a LaTeX project, too. So I eventually
;; need a LaTeX export.
(defun as/markdown-region-to-latex (start end)
  (interactive "r")
  (goto-char start)
  (save-restriction
    (let (in-list skip-to)
      (narrow-to-region start end)
      (while (re-search-forward "\\*\\|\n\\|\\`" nil t)
	(goto-char (match-beginning 0))
	(if (= (point) (match-end 0))
	    (setq skip-to (1+ (point)))
	  (setq skip-to (match-end 0)))
	(cond ((looking-at "\\*\\*\\b\\([^*]*?\\)\\b\\*\\*")
	       (replace-match "\\\\textbf{\\1}"))
	      ((looking-at "\\*\\b\\([^*]*?\\)\\b\\*")
	       (replace-match "\\\\textit{\\1}"))
	      ((looking-at "^# \\(.*\\)")
	       (replace-match "\\\\section{\\1}"))
	      ((looking-at "^## \\(.*\\)")
	       (replace-match "\\\\subsection{\\1}"))
	      ((looking-at "^### \\(.*\\)")
	       (replace-match "\\\\subsubsection{\\1}"))
	      ((looking-at "^\\* ")
	       (replace-match (if in-list "\\\\item " "\\\\begin{itemize}\n\\\\item "))
	       (setq in-list "itemize"))
	      ((looking-at "^[0-9]+\\. ")
	       (replace-match (if in-list "\\\\item " "\\\\begin{enumerate}\n\\\\item "))
	       (setq in-list "enumerate"))
	      ((and in-list (looking-at "^"))
	       (replace-match (format "\\\\end{%s}\n" in-list))
	       (setq in-list nil))
	      (t (goto-char skip-to)))))))
