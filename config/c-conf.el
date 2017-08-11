;; bitblee uses K&R with tabs and 120 columns
(add-hook 'c-mode-hook 'asc:init-c-mode)

(defun asc:init-c-mode ()
  (idle-highlight-mode 1)
  (local-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (setq c-basic-offset 8
	set-fill-column 120))

(eval-after-load "cc-vars"
  (lambda ()
    (add-to-list 'c-default-style '(c-mode . "k&r"))))

(setq tags-revert-without-query t)
