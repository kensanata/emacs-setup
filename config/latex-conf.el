;; limit to the commands I need
(setq tex-compile-commands
      '(("latexmk -pvc -view=none -bibtex- -pdf %f" t "%r.pdf")
	("skim $f")
	("pdflatex  %f" t "%r.pdf")
	("makeindex %r" "%r.idx" "%r.ind")))

(setq reftex-index-macros
      '(("\\key[]{*}" "idx" ?k "" nil nil)
	("\\hiref[]{}{*}" "idx" ?k "" nil nil)
	("\\animal{*}" "idx" ?k "" t nil)
	("\\ruins{*}" "idx" ?k "" t nil)
	("\\jungles{*}" "idx" ?k "" t nil)
	("\\deserts{*}" "idx" ?k "" t nil)
	("\\forests{*}" "idx" ?k "" t nil)
	("\\mountains{*}" "idx" ?k "" t nil)
	("\\caves{*}" "idx" ?k "" t nil)
	("\\savannas{*}" "idx" ?k "" t nil)
	("\\swamps{*}" "idx" ?k "" t nil)))

(add-hook 'latex-mode-hook 'asc:latex-mode-setup)

(defun asc:latex-mode-setup ()
  "See `font-lock-keywords' for some explanation.
This modifies `latex-font-lock-keywords'."
  
  ;; C-c =  toc
  ;; C-c (  label
  ;; C-c )  reference
  ;; C-c <  index
  ;; C-c >  display index
  (reftex-mode 1)
  ;; (setq reftex-label-alist nil)
  (add-to-list 'reftex-label-alist
	       '("\\animal{*}" ?a "animal:" "%s" t nil 2))

  (flyspell-mode 1)
  (auto-fill-mode 1)
  
  (local-set-key (kbd "C-c t") 'asc:latex-insert-tag)
  (local-set-key (kbd "C-z") 'tex-shell-here)
  (font-lock-add-keywords
   nil
   '(("\\\\key\\(?:\\[\\(.*?\\)\\]\\)?{\\(.*?\\)}"
      (2 'bold))
     ("\\\\hyperref\\[\\([^]]*\\)\\]{\\([^}]*\\)}"
      ;; (1 'underline)
      (2 'bold))
     ;; ("\\\\href{\\([^}]*\\)}{\\([^}]*\\)}"
     ;;  (1 'underline)
     ;;  (2 'bold))
     )))

(defun tex-shell-here ()
  (interactive)
  (unless (get-buffer "*tex-shell*")
    (tex-start-shell))
  (pop-to-buffer "*tex-shell*"))

(defvar asc:latex-insert-tag-defaults
  '("emph" "textbf" "key" "href{}" "hyperref[]"))

(defvar asc:latex-insert-tag-history nil)

(defun asc:latex-insert-tag ()
  (interactive)
  (let (start end)
    (if (use-region-p)
	(setq start (region-beginning)
	      end (region-end))
      (setq start (if (looking-at "\\b\\w")
		      (point)
		    (backward-word 1)
		    (point))
	    end (progn
		  (forward-word 1)
		  (point))))
    (goto-char end)
    (insert "}")
    (goto-char start)
    (insert "\\{")
    (backward-char 1)
    (insert (completing-read "Tag: " asc:latex-insert-tag-defaults))
    (when (or (= (char-before) ?\])
	      (= (char-before) ?\}))
      (backward-char 1))))
