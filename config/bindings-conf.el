(setq default-input-method 'german-prefix)

(asc:package-install 'expand-region)

(global-set-key (kbd "C-'") 'er/expand-region)

(global-set-key (kbd "C-<tab>") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
	try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(define-key minibuffer-local-map (kbd "C-<tab>") 'hippie-expand)

(global-set-key (kbd "C-x 8 1 / 3") "⅓")
(global-set-key (kbd "C-x 8 1 / 5") "⅕")
(global-set-key (kbd "C-x 8 1 / 6") "⅙")
(global-set-key (kbd "C-x 8 1 / 8") "⅛")
(global-set-key (kbd "C-x 8 2 / 3") "⅔")
(global-set-key (kbd "C-x 8 2 / 5") "⅖")
(global-set-key (kbd "C-x 8 3 / 5") "⅗")
(global-set-key (kbd "C-x 8 4 / 5") "⅘")
(global-set-key (kbd "C-x 8 5 / 6") "⅚")
(global-set-key (kbd "C-x 8 <right>") "→")
(global-set-key (kbd "C-x 8 <left>") "←")
(global-set-key (kbd "C-x 8 <up>") "↑")
(global-set-key (kbd "C-x 8 <down>") "↓")
(global-set-key (kbd "C-x 8 [") "｢")
(global-set-key (kbd "C-x 8 ]") "｣")
