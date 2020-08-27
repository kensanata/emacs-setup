(use-package swiper)

(ivy-mode 1)

(global-set-key (kbd "C-o") 'swiper);; instead of open-line (mnemonic: "occur")

(setq projectile-completion-system 'ivy)

;; This takes makes editing LaTeX very slow
;; (global-flycheck-mode 1)
;; (global-set-key (kbd "C-c ,") #'avy-flycheck-goto-error)
