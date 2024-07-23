(use-package haskell-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.tidal\\'" . tidal-mode))
(autoload 'tidal-mode "tidal")
