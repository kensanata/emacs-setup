;; bitblee uses K&R with tabs and 120 columns
(add-hook 'c-mode-hook 'asc:init-c-mode)

(defun asc:init-c-mode ()
  (idle-highlight-mode 1)
  (local-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (setq c-basic-offset 8
	fill-column 120))

(eval-after-load "cc-vars"
  (lambda ()
    (add-to-list 'c-default-style '(c-mode . "k&r"))))

(setq tags-revert-without-query t)

(define-abbrev c-mode-abbrev-table "co" "" 'asc:c-comment)

(define-skeleton asc:c-comment
  "Insert a multi-line comment.

/**
 * Like this.
 */"
  ?\n
  "/**\n"
  " * " _ "\n"
  " */\n")
