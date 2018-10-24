(setq nxml-child-indent 0)
(add-to-list 'auto-mode-alist '("\\.opml\\'" . nxml-mode))

(add-hook 'nxml-mode-hook 'asc:init-nxml-mode)

(defun asc:init-nxml-mode ()
  (idle-highlight-mode 1)
  (local-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (define-abbrev nxml-mode-abbrev-table "ol"
    "" 'opml-outline)
  (setq indent-tab-mode nil
	c-basic-offset 2
	truncate-lines t))

(define-skeleton opml-outline
  "Add a new entry to an OPML outline."
  "Feed: "
  "<outline title=\"" _ "\" xmlUrl=\"" str "\"/>")
