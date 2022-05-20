(add-hook 'html-mode-hook 'asc:init-html-mode)

(defun asc:init-html-mode ()
  (idle-highlight-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (setq c-basic-offset 2
	tab-width 2))

;; This is not working out for me.
;; 
;; (eval-after-load 'sgml-mode
;;   '(progn
;;      (require 'tagedit)
;;      (tagedit-add-paredit-like-keybindings)
;;      (add-hook 'html-mode-hook (lambda ()
;; 				 (tagedit-mode 1)
;; 				 (tagedit-add-experimental-features)))))
