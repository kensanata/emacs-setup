(asc:package-install 'swiper)

(ivy-mode 1)

(global-set-key (kbd "C-s") 'swiper);; instead of isearch
(global-set-key (kbd "C-r") 'swiper);; instead of isearch-backward

(setq projectile-completion-system 'ivy)

(global-flycheck-mode 1)
(global-set-key (kbd "C-c ,") #'avy-flycheck-goto-error)
