(asc:package-install 'swiper)

(ivy-mode 1)

(global-set-key (kbd "C-o") 'swiper);; instead of open-line (mnemonic: "occur")

(setq projectile-completion-system 'ivy)

(global-flycheck-mode 1)
(global-set-key (kbd "C-c ,") #'avy-flycheck-goto-error)
