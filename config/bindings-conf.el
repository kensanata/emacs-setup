(global-set-key (kbd "C-'") 'er/expand-region)

(global-set-key (kbd "C-<tab>") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
	try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(define-key minibuffer-local-map (kbd "C-<tab>") 'hippie-expand)
