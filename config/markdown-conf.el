(use-package markdown-mode
  :mode ("\\.\\(?:md\\|markdown\\)\\'" . markdown-mode)
  :config
  (defalias
    'markdown-add-xhtml-header-and-footer
    'as/markdown-add-xhtml-header-and-footer))

(setq markdown-enable-wiki-links t)

(add-hook 'markdown-mode-hook 'asc:markdown-init)

(defun asc:markdown-init ()
  (local-set-key (kbd "C-c C-c C-p") 'asc:markdown-publish))

(advice-add 'markdown-link-url :filter-return #'asc:markdown-link-url)

(defun asc:markdown-link-url (url)
  "Add .md to URL if that makes sense."
  (let* ((filename (url-unhex-string url))
         (markdown (concat filename ".md")))
    (cond ((file-exists-p filename)
           filename)
          ((file-exists-p markdown)
           markdown)
          (t url))))

(defun asc:markdown-publish (name add-changes)
  "Publish the page on my blog."
  (interactive
   (list (read-string "Name of the page: "
                      (string-replace ".md" "" (buffer-name)))
         (y-or-n-p "Add to list of changes: ")))
  (let* ((url (concat "https://alexschroeder.ch/save/" name))
         (url-request-method "POST")
         (url-request-coding-system 'utf-8)
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8")
            ("Connection" . "close")))
         (url-request-data
          (mm-url-encode-www-form-urlencoded
           `(("body" . ,(buffer-substring-no-properties (point-min) (point-max)))
             ("notify" . ,(if add-changes "on" ""))))))
    (when (url-basic-auth url)
      (add-to-list 'url-request-extra-headers
                   (cons "Authorization" (url-basic-auth url))))
    (url-retrieve url (lambda (status &rest ignore)
                        (let ((kill-this-buffer (current-buffer)))
                          (if (and (integerp status) (not (< status 300)))
                              (error "Oh no: %d 😱" status)
                            (message "No idea if this worked")))))))

;; The text files in my Dropbox folder are Markdown files.
(add-to-list 'auto-mode-alist '("/Dropbox/.*\\.txt\\'" . markdown-mode))

;; skip first entry of TOC (which is going to be the document title)
(setq markdown-toc-user-toc-structure-manipulation-fn
      'as/markdown-without-main-title)

(defun as/markdown-without-main-title (toc-structure)
  "Skip the first entry and decrease the level of all other entries."
  (setq toc-structure (cdr toc-structure))
  (dolist (item toc-structure)
    (setcar item (1- (car item))))
  toc-structure)

;; The Markdown files I write using iA Writer use newlines to separate
;; paragraphs. That's why I need Visual Line Mode. I also need to
;; disable M-q. If I fill paragraphs, that introduces unwanted
;; newlines.
(defun as/ia-writer ()
  (visual-line-mode 1)
  (local-set-key (kbd "M-q") 'ignore))

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
