;; noccur-dired: Perform ‘multi-occur’ with REGEXP in all dired marked files.
;; noccur-project: Perform ‘multi-occur’ with REGEXP in the current project files.
;; (use-package noccur)

;; use + and - in occur mode
(use-package occur-context-resize
  :ensure t)

(use-package color-moccur
  :ensure t
  :bind ("C-c O" . multi-occur-in-matching-buffers))
