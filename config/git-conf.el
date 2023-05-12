(use-package magit :ensure t
  :bind ("C-c o" . magit))

(use-package git-gutter :ensure t)
(global-git-gutter-mode +1)

;; always follow links
(setq vc-follow-symlinks t)
