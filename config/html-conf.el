(add-hook 'html-mode-hook 'asc:init-html-mode)

(defun asc:init-html-mode ()
  (idle-highlight-mode 1)
  (local-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (setq c-basic-offset 2
	tab-width 2))
