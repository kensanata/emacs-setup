(use-package zeal-at-point
  :ensure t
  :bind ("C-c h" . zeal-at-point)
  :config (add-to-list 'zeal-at-point-mode-alist '(nxml-mode . ("svg" "css"))))
