(use-package raku-mode)

(add-hook 'raku-mode-hook 'as/perl6-init)

(defun as/perl6-init ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (idle-highlight-mode 1)
  (setq tab-width 4
	indent-tabs-mode nil)
  (eldoc-mode 1))
