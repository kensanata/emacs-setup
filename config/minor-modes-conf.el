;; deactivate some bling
(dolist (mode '(blink-cursor-mode
		tool-bar-mode
		menu-bar-mode
		scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; active some other bling
;; (toggle-frame-fullscreen)
(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 100) (height . 25)))
(show-paren-mode 1)
(winner-mode 1)
(windmove-default-keybindings)
(column-number-mode 1)

;; I tried it, but I didn't get used to it.
;; (electric-pair-mode 1)

;; (auto-image-file-mode 1) -- corrupts SVG files containing UTF-8?

;; settings
(setq visible-bell t
      completion-ignore-case t
      read-buffer-completion-ignore-case t)
(setq sentence-end-double-space nil)
(put 'narrow-to-region 'disabled nil)
