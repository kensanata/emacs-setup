;; deactivate some bling
(dolist (mode '(blink-cursor-mode
		tool-bar-mode
		blink-cursor-mode
		scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; active some other bling
(show-paren-mode 1)
(winner-mode 1)
(windmove-default-keybindings)
(column-number-mode 1)
(electric-pair-mode 1)

;; (auto-image-file-mode 1) -- corrupts SVG files containing UTF-8?

;; settings
(setq visible-bell t
      completion-ignore-case t
      read-buffer-completion-ignore-case t)
(setq sentence-end-double-space nil)
(put 'narrow-to-region 'disabled nil)
