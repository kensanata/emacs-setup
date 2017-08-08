(add-hook 'c-mode-hook 'idle-highlight-mode)

;; bitblee uses K&R with tabs and 120 columns
(add-hook 'c-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-<tab>") 'command-complete)
	    (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
	    (setq c-basic-offset 8
		  set-fill-column 120)))

(eval-after-load "cc-vars"
  (lambda ()
    (add-to-list 'c-default-style '(c-mode . "k&r"))))
