(asc:package-install 'markdown-mode)

;; The text files in my Dropbox folder are Markdown files.
(add-to-list 'auto-mode-alist '("/Dropbox/.*\\.txt\\'" . markdown-mode))

;; The Markdown files I write using IA Writer use newlines to separate
;; paragraphs. That's why I need Visual Line Mode. I also need to
;; disable M-q. If I fill paragraphs, that introduces unwanted
;; newlines.
(defun as/ia-writer ()
  (visual-line-mode 1)
  (local-set-key (kbd "M-q") 'ignore))

(eval-after-load "markdown-mode"
  '(defalias
     'markdown-add-xhtml-header-and-footer
     'as/markdown-add-xhtml-header-and-footer))

(defun as/markdown-add-xhtml-header-and-footer (title)
    "Wrap XHTML header and footer with given TITLE around current buffer."
    (goto-char (point-min))
    (insert "<!DOCTYPE html5>\n"
	    "<html>\n"
	    "<head>\n<title>")
    (insert title)
    (insert "</title>\n")
    (insert "<meta charset=\"utf-8\" />\n")
    (when (> (length markdown-css-paths) 0)
      (insert (mapconcat 'markdown-stylesheet-link-string markdown-css-paths "\n")))
    (insert "\n</head>\n\n"
	    "<body>\n\n")
    (goto-char (point-max))
    (insert "\n"
	    "</body>\n"
	    "</html>\n"))

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
      (while (and (re-search-forward "\\*\\|^\\|\\`" nil t)
		  (or (not skip-to) (>= (point) skip-to)))
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
	      ((and in-list (looking-at "^.\\|\\'"))
	       (insert (format "\\end{%s}\n" in-list))
	       (setq in-list nil))
	      (t (goto-char skip-to))))
      (goto-char start)
      (while (search-forward " " nil t)
	(replace-match "~")))))
