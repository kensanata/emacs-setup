;; minor modes
(blink-cursor-mode -1)
(ivy-mode 1)
(setq visible-bell t)
(tool-bar-mode -1)
(show-paren-mode 1)
(winner-mode 1)
(windmove-default-keybindings)
(column-number-mode 1)
(blink-cursor-mode -1)
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; (auto-image-file-mode 1) -- corrupts SVG files containing UTF-8?

;; settings
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t)
(setq sentence-end-double-space nil)
(put 'narrow-to-region 'disabled nil)
