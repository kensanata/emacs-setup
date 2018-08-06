;; bitblee uses K&R with tabs and 120 columns
(add-hook 'c-mode-hook 'asc:init-c-mode)

(defun asc:init-c-mode ()
  (idle-highlight-mode 1)
  (local-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (define-abbrev c-mode-abbrev-table "fu" "" 'asc:c-comment-function)
  (define-abbrev c-mode-abbrev-table "co" "" 'asc:c-comment)
  (setq c-basic-offset 4
	tab-width 4
	fill-column 120))

(eval-after-load "cc-vars"
  (lambda ()
    (add-to-list 'c-default-style '(c-mode . "k&r"))))

(setq tags-revert-without-query t)

(define-skeleton asc:c-comment-function
  "Insert a multi-line comment.

/**
 * Like this.
 */"
  nil
  "/**\n"
  " * " _ "\n"
  " */")

(define-skeleton asc:c-comment
  "Insert a single-line comment. /* like this */"
  nil "/* " _ " */")
